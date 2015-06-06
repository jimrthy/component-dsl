(ns component-dsl.system
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]
            [com.stuartsierra.component :as component]
            [schema.core :as s])
  (:import [com.stuartsierra.component SystemMap]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema

(def system-structure
  "Component 'name' to the namespaced initialization function"
  {s/Keyword s/Symbol})

(def system-dependencies
  "Component 'name' to a seq of the 'names' of its dependencies"
  {s/Keyword [s/Keyword]})

(def system-description
  "Describe the system structure and its dependencies"
  {:structure system-structure
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
  {s/Keyword s/Any})

(def configuration-tree
  "Which parameters get passed to the various constructors?

The keys are the corresponding names of each Component.

This seems like a silly approach...why not just describe
each Component, include its constructor and associated args,
and its dependencies altogether?

The entire point is really to make all those details
declarative and easy, then to merge them into that sort
of single map in here."
  {s/Keyword (s/maybe option-map)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internals

(s/defn ^:always-validate initialize :- s/Any  ; named-instances...this is tricky
  "require the individual namespaces and call each Component's constructor,
returning a seq of name/instance pairs that probably should have been a map"
  [descr :- system-structure
   config-options :- configuration-tree]
  (let [result
        (mapcat (fn [[name ctor-sym]]
                  ;; Called for the side-effects
                  (-> ctor-sym namespace symbol require)
                  (let [ctor (resolve ctor-sym)
                        options (name config-options)
                        instance (ctor config-options)]
                    [name instance]))
                descr)]
    (println "Initialized System:\n"
             (with-out-str (pprint result)))
    result))

(s/defn ^:always-validate system-map :- SystemMap
  [descr :- system-structure
   config-options :- configuration-tree]
  (let [inited (initialize descr config-options)]
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
  (let [options (if (seq config-options)
                  (first config-options)
                  {})
        descr (load-resource config-file-name)]
    (build descr options)))
