(ns component-dsl.system
  (:require [#?(:clj clojure.edn) #?(:cljs cljs.reader) :as edn]
            #_[clojure.java.io :as io]
            [#?(:clj clojure.pprint) #?(:cljs cljs.pprint) :refer (pprint)]
            [clojure.spec :as s]
            [com.stuartsierra.component :as component #?@(:cljs [:refer [SystemMap]])]
            [component-dsl.management :as mgt])
  #?(:clj (:import [com.stuartsierra.component SystemMap])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specs

(s/def ::component-name keyword?)
;; When a Component's dependency name doesn't match what the System calls it
;; Key is what your Component calls it, value is what the System does
(s/def ::component-name-map (s/map-of keyword? keyword?))

(s/def component-ctor-id symbol?)

;; Component 'name' to the namespaced initialization function
(s/def ::initialization-map (s/map-of ::component-name ::component-ctor-id))

(s/def ::dependency-seq (s/or :seq ::component-name
                              :map ::component-name-map))
;; Component 'name' to a seq of the 'names' of its dependencies
(s/def ::system-dependencies (s/map-of ::component-name ::dependency-seq))

;; Describe the system structure and its dependencies
(s/def ::system-description (s/keys ::req {::structure ::initialization-map  ; this is a poor naming choice
                                           ;; Very tempting to make this optional
                                           ;; It won't be, except for trivial systems.
                                           ;; But it's annoying to remember for those.
                                           ;; Ah, well. If you're writing something trivial, don't use this.
                                           ::dependencies ::system-dependencies}))

;; key-value pairs that are suitable for passing to dependencies
;; as the last argument of apply.
(s/def ::named-instances (s/cat :name keyword?
                                :instance any?))

;; The options to supply to each component when it's constructed
;; For flexibility, everything should be legal here.
;; But, come on, when would the keys ever be anything except
;; keywords?
(s/def ::option-map (s/map-of ::component-name any?))

;; Which parameters get passed to the various constructors?
;;
;; The keys are the corresponding names of each Component.
;;
;; This seems like a silly approach...why not just describe
;; each Component, include its constructor and associated args,
;; and its dependencies altogether?
;;
;; The entire point is really to make all those details
;; declarative and easy, then to merge them into that sort
;; of single map in here.
(s/def ::configuration-tree (s/map-of keyword? ::option-map))

(s/def ::name-space symbol?)
(s/def ::schema-name symbol?)

;; An individual description
;; TODO: Is there schema for describing legal schema anywhere?
;; Q: Where is this actually used?
(s/def ::schema any?)

;; Really just so I have a meaningful name to call these things
;; TODO: This name seemed cute, but it means exactly the opposite of
;; what I intended (it's the weird singular form rather than the plural)
(s/def ::schemata
  (s/coll-of ::schema))

;; Really just a map of symbols marking a namespace to
;; symbols naming schemata in that namespace
(s/def ::schema-description
  (s/map-of ::name-space (s/or :single ::schema-name
                               :seq (s/coll-of ::schema-name))))

;; TODO: Try redefining ComponentName as this
;; I'm just not sure that'll work in non-sequences
;; (such as when I'm using it as the key in a map)
(s/def ::component-instance-name
  keyword?)
(s/def ::component-instance any?)
(s/def ::component (s/cat ::component-instance-name ::component-instance))

(s/def ::system-map #(instance? SystemMap %))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internals

(s/fdef load-var
        :args (s/cat :namespace ::name-space
                     :var-name symbol?)
        :ret (s/nilable any?))
(defn load-var
  "Get the value of var inside namespace"
  [namespace var-name]
  (let [sym (symbol (str namespace "/" var-name))]
    (try
      ;; TODO: This is *totally* not cross-platform
      #?(:clj (eval sym))
      #?(:cljs (throw (ex-info "not implemented"
                               {:problem "eval *is* available; it's just ugly"})))
      (catch #?(:clj RuntimeException) #?(:cljs :default) ex
        (throw (ex-info
                (str "Loading " var-name " from " namespace
                     " (which amounts to " sym ") "
                     " failed")
                {:problem var-name
                :reason ex}))))))

(s/fdef require-schematic-namespaces! :args (s/cat :descr ::schema-description))
(defn require-schematic-namespaces!
  "Makes sure all the namespaces where the schema
are found are available, so we can access the schema"
  [description]
  (dorun (map #?(:clj require)
              #?(:cljs (throw (ex-info "This exists now; just need the will to use it" {})))
              (keys description))))

(comment
  (mapcat (fn [[k v]]
            (println "Extracting" v "from ns" k)
            (if (symbol? v)
              (load-var k v)
              (mapcat (partial load-var k) v)))
          {'one '[schema-a schema-b]
           'two 'schema-a}))
(s/fdef extract-schema
        :args (s/cat :descriptions ::schema-description)
        :ret ::schemata)
(defn extract-schema
  "Returns a seq of the values of the vars in each namespace"
  [descriptions]
  (mapcat (fn [[namespace var-name]]
            (if (symbol? var-name)
              [(load-var namespace var-name)]
              (map (partial load-var namespace) var-name)))
          descriptions))

(s/fdef translate-schematics!
        :args (s/cat :description ::schema-description)
        :ret ::schemata)
(defn translate-schematics!
  "require the namespace and load the schema specified in each.

N.B. Doesn't even think about trying to be java friendly. No defrecord!"
  [description]
  (require-schematic-namespaces! description)
  (extract-schema description))

;;; Q: What's the spec equivalent of schema's ^:always-validate?
(s/fdef initialize
        :args (s/cat :descr ::initialization-map
                     :config-options ::configuration-tree)
        :ret (s/coll-of ::component))
(defn initialize
  "require the individual namespaces and call each Component's constructor,
returning a seq of name/instance pairs that probably should have been a map"
  [descr config-options]
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

;; TODO: This is another to always validate
(s/fdef system-map
        :args (s/cat :descr ::initialization-map
                     :config-options ::configuration-tree)
        :ret ::system-map)
(defn system-map
  [descr config-options]
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

(s/fdef dependencies
        :args (s/cat :inited ::system-map
                     :descr ::system-dependencies)
        :ret ::system-map)
(defn dependencies
  "Add the system's dependency tree"
  [inited descr]
  (comment) (println "Preparing to build dependency tree for\n"
                     (with-out-str (pprint inited))
                     "based on dependency tree\n"
                     (with-out-str (pprint descr)))
  (component/system-using inited descr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

;; TODO: ^:always-validate
(s/fdef build
        :args (s/cat :descr ::system-description
                     :options ::configuration-tree)
        :ret ::system-map)
(defn  build
  "Returns a System that's ready to start"
  [descr options]
  (let [pre-init (system-map (:structure descr) options)]
    (dependencies pre-init (:dependencies descr))))

(s/fdef ctor
        :args (s/cat :config-file-name string?
                     :config-options ::configuration-tree)
        :ret ::system-map)
(defn ctor
  "Translates an external EDN resource that describes a system into one that's ready to start

config-file name needs to point to an EDN file w/ a description map.
The :structure is a map of component identifiers to their constructors.
The :dependencies describes the dependencies among components
Options is a map of maps that will be supplied to each constructor

This seemed like a good idea once upon a time, but it's looking more
and more dubious as I keep moving forward and ignoring it."
  [config-file-name
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
