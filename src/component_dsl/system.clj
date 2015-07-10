(ns component-dsl.system
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]
            [com.stuartsierra.component :as component]
            [schema.core :as s])
  (:import [com.stuartsierra.component SystemMap]))

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
   :dependencies system-dependencies})

(def named-instances
  "key-value pairs that are suitable for passing to dependencies
as the last argument of apply.

TODO: How do I specify that in schema?"
  [;; Neither of these options work.
   ;; They're really both expecting the same thing.
   ;; To get this right, I think I'd be forced to
   ;; write a custom schema Walker.
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
  "Really just so I have a meaningful name to call these things"
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
      (eval sym)
      (catch RuntimeException ex
        ;; Logger isn't initialized yet

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
  (dorun (map require (keys d))))

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
               (-> ctor-sym namespace symbol require)
               (if-let [ctor (resolve ctor-sym)]
                 (let [
                       ;; Don't force caller to remember this;
                       ;; even though it really is a required
                       ;; part of the protocol/API. Leaving it
                       ;; off is just begging for errors
                       local-options (get config-options name {})
                       instance
                       (try (ctor local-options)
                            (catch NullPointerException ex
                              (let [msg (str ex
                                             "\nTrying to call ctor "
                                             ctor-sym
                                             "\nwith params:\n"
                                             local-options
                                             "\nHonestly, this is fatal")]
                                (throw (RuntimeException. msg ex)))))]
                   [name instance])
                 (throw (RuntimeException. (str "No such constructor:\n"
                                                ctor-sym)))))
             descr)]
    (comment (println "Initialized System:\n"
                      (with-out-str (pprint result))))
    result))

(s/defn ^:always-validate system-map :- SystemMap
  [descr :- initialization-map
   config-options :- configuration-tree]
  (let [inited-pairs (initialize descr config-options)
        inited (apply concat inited-pairs)]
    (apply component/system-map inited)))

(s/defn ^:always-validate load-resource :- s/Any
  [url :- s/Str]  ; Actually, this should probably accept a URI
  (-> url
      clojure.java.io/resource
      slurp
      edn/read-string))

(s/defn dependencies :- SystemMap
  "Add the system's dependency tree"
  [inited :- SystemMap
   descr :- system-dependencies]
  (comment (log/debug "Preparing to build dependency tree for\n"
                      (with-out-str (pprint inited))
                      "based on the dependency description\n"
                      (with-out-str (pprint descr))))
  (component/system-using inited descr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(s/defn build :- SystemMap
  "Returns a System that's ready to start"
  [descr :- system-description
   options :- option-map]
  (let [pre-init (system-map (:structure descr) options)]
    (dependencies pre-init (:dependencies descr))))

(s/defn ctor :- SystemMap
  "config-file name needs to point to an EDN file w/ a description map.
The :structure is a map of component identifiers to their constructors.
The :dependencies describes the dependencies among components
Options is a map of maps that will be supplied to each constructor"
  [config-file-name :- s/Str
   & config-options]
  (let [options (if (seq config-options)  ; the way optionals work
                  (first config-options)
                  {})
        descr (load-resource config-file-name)]
    (build descr options)))
