(ns component-dsl.system
  (:require [#?(:clj clojure.edn) #?(:cljs cljs.reader) :as edn]
            #_[clojure.java.io :as io]
            [#?(:clj clojure.pprint) #?(:cljs cljs.pprint) :refer (pprint)]
            [com.stuartsierra.component :as component #?@(:cljs [:refer [SystemMap]])]
            [component-dsl.management :as mgt]
            [schema.core :as s])
  #?(:clj (:import [com.stuartsierra.component SystemMap])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema

(def component-name s/Keyword)

(def component-ctor-id s/Symbol)

(def initialization-map
  "Component 'name' to the namespaced initialization function"
  {component-name component-ctor-id})

(def system-dependencies
  "Component 'name' to a seq of the 'names' of its dependencies"
  {component-name [component-name]})

(def system-description
  "Describe the system structure and its dependencies"
  {:structure initialization-map   ; this is a poor naming choice
   ;; Very tempting to make this optional
   ;; It won't be, except for trivial systems.
   ;; But it's annoying to remember for those.
   ;; Ah, well. If you're writing something trivial, don't use this.
   :dependencies system-dependencies})

(def named-instances
  "key-value pairs that are suitable for passing to dependencies
as the last argument of apply.

TODO: How do I specify that in schema?"
  [;; Neither of these options work.
   ;; They're really both expecting the same thing.
   ;; To get this right, I think I'd be forced to
   ;; write a custom schema Walker.
   ;; The trouble is that I really require an alternating pattern:
   ;; [s/Keyword s/Any...]
   ;; I can regulate my API to something that fits their patterns, but
   ;; it would be nice to be explicit about what I'm trying to pass
   ;; along, if only because I get it wrong so frequently
   #_(s/pair s/Keyword "name" s/Any "instance")
   #_[(s/one s/Keyword "name") (s/one s/Any "instance")]
   ])

(def option-map
  "The options to supply to each component when it's constructed"
  ;; For flexibility, everything should be legal here.
  ;; But, come on, when would the keys ever be anything except
  ;; keywords?
  {component-name s/Any})

(def configuration-tree
  "Which parameters get passed to the various constructors?

The keys are the corresponding names of each Component.

This seems like a silly approach...why not just describe
each Component, include its constructor and associated args,
and its dependencies altogether?

The entire point is really to make all those details
declarative and easy, then to merge them into that sort
of single map in here."
  {s/Keyword option-map})

(def name-space s/Symbol)
(def schema-name s/Symbol)

(def schema
    "An individual description
TODO: Is there schema for describing legal schema anywhere?"
    s/Any)

(def schemata
  "Really just so I have a meaningful name to call these things
TODO: This name seemed cute, but it means exactly the opposite of
what I intended (it's the weird singular form rather than the plural)"
  [schema])

(def schema-description
    "Really just a map of symbols marking a namespace to
symbols naming schemata in that namespace"
    {name-space (s/either schema-name [schema-name])})

(def component-instance-name
  "TODO: Try redefining ComponentName as this
  I'm just not sure that'll work in non-sequences
  (such as when I'm using it as the key in a map)"
  (s/one s/Keyword "name"))
(def component-instance (s/one s/Any "instance"))
;; Note that this declaration does not match reality:
;; This is really just an alternating seq of these pairs,
;; like a common lisp alist.
(def component [component-instance-name component-instance])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internals

(s/defn load-var :- s/Any
  "Get the value of var inside namespace"
  [namespace :- name-space
   var-name :- s/Symbol]
  (let [sym (symbol (str namespace "/" var-name))]
    (try
      ;; TODO: This is *totally* not cross-platform
      #?(:clj (eval sym))
      #?(:cljs (throw (ex-info "eval *is* available; it's just ugly" {})))
      (catch #?(:clj RuntimeException) #?(:cljs :default) ex
        (throw (ex-info
                (str "Loading " var-name " from " namespace
                     " (which amounts to " sym ") "
                     " failed")
                {:problem var-name
                :reason ex}))))))

(s/defn require-schematic-namespaces!
  "Makes sure all the namespaces where the schemata
are found are available, so we can access the schemata"
  [d :- schema-description]
  (dorun (map #?(:clj require)
              #?(:cljs (throw (ex-info "This exists now; just need the will to use it" {})))
              (keys d))))

(comment
  (mapcat (fn [[k v]]
            (println "Extracting" v "from ns" k)
            (if (symbol? v)
              (load-var k v)
              (mapcat (partial load-var k) v)))
          {'one '[schema-a schema-b]
           'two 'schema-a}))
(s/defn extract-schema :- schemata
  "Returns a seq of the values of the vars in each namespace"
  [d :- schema-description]
  (mapcat (fn [[namespace var-name]]
            (if (symbol? var-name)
              [(load-var namespace var-name)]
              (map (partial load-var namespace) var-name)))
          d))

(s/defn translate-schematics! :- schemata
  "require the namespace and load the schema specified in each.

N.B. Doesn't even think about trying to be java friendly. No defrecord!"
  [d :- schema-description]
  (require-schematic-namespaces! d)
  (extract-schema d))

(s/defn ^:always-validate initialize :- [component]
  "require the individual namespaces and call each Component's constructor,
returning a seq of name/instance pairs that probably should have been a map"
  [descr :- initialization-map
   config-options :- configuration-tree]
  (let [result
        (map (fn [[name ctor-sym]]
               ;; Called for the side-effects
               ;; This should have been taken care of below,
               ;; in the call to require-schematic-namespaces!
               ;; But better safe than sorry.
               ;; TODO: Trust the original require-schematic-namespaces!
               ;; for this step. Really
               ;; don't want to put this sort of side-effect in
               ;; the middle of a big nasty function like this.
               (->> ctor-sym namespace symbol #?(:clj require) #?(:cljs (throw (ex-info "That exists. Use it" {}))))
               (if-let [ctor (#?(:clj resolve) #?(:cljs (throw (ex-info "This can't possibly work as-is...what's the equivalent?" {}))) ctor-sym)]
                 (let [
                       ;; Don't force caller to remember this;
                       ;; even though it really is a required
                       ;; part of the protocol/API. Leaving it
                       ;; off is just begging for errors
                       local-options (get config-options name {})
                       instance
                       (try (ctor local-options)
                            (catch #?(:clj NullPointerException)
                                #?(:cljs :default)  ; Q: What about other platforms?
                                ex
                              (let [msg (str ex
                                             "\nTrying to call ctor "
                                             ctor-sym
                                             "\nwith params:\n"
                                             local-options
                                             "\nHonestly, this is fatal")]
                                (throw (#?(:clj RuntimeException.) #?(:cljs js/Error.) msg ex)))))]
                   [name instance])
                 (throw (#?(:clj RuntimeException.) #?(:cljs js/Error.) (str "No such constructor:\n"
                                                                             ctor-sym)))))
             descr)]
    (comment (println "Initialized System:\n"
                      (with-out-str (pprint result))))
    result))

(s/defn ^:always-validate system-map :- SystemMap
  [descr :- initialization-map
   config-options :- configuration-tree]
  (println "cpt-dsl::system/system-map -- Initializing system\n"
           (with-out-str (pprint descr))
           "with options:\n"
           (with-out-str (pprint config-options)))
  (let [inited-pairs (initialize descr config-options)
        inited (apply concat inited-pairs)
        result (apply component/system-map inited)]
    (println "cpt-dsl::system/system-map:\nInited-pairs:\n"
             (with-out-str (pprint inited-pairs))
             "initialized:\n"
             (with-out-str (pprint inited-pairs))
             "result:\n"
             (with-out-str (pprint result)))
    result))

(s/defn dependencies :- SystemMap
  "Add the system's dependency tree"
  [inited :- SystemMap
   descr :- system-dependencies]
  (comment) (println "Preparing to build dependency tree for\n"
                     (with-out-str (pprint inited))
                     "based on dependency tree\n"
                     (with-out-str (pprint descr)))
  (component/system-using inited descr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(s/defn ^:always-validate build :- SystemMap
  "Returns a System that's ready to start"
  [descr :- system-description
   options :- configuration-tree]
  (let [pre-init (system-map (:structure descr) options)]
    (dependencies pre-init (:dependencies descr))))

(s/defn ctor :- SystemMap
  "Translates an external EDN resource that describes a system into one that's ready to start

config-file name needs to point to an EDN file w/ a description map.
The :structure is a map of component identifiers to their constructors.
The :dependencies describes the dependencies among components
Options is a map of maps that will be supplied to each constructor

This seemed like a good idea once upon a time, but it's looking more
and more dubious as I keep moving forward and ignoring it."
  [config-file-name :- s/Str
   & config-options]
  (let [options (if (seq config-options)  ; the way optionals work
                  (first config-options)
                  {})
        descr (mgt/load-resource config-file-name)]
    (build (dissoc descr :options)
           ;; Let config file override whatever caller supplies
           ;; This seems dubious, but it's easier to change a config
           ;; file than code.
           (into options (:options descr)))))
