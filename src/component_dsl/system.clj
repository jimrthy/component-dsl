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

(def option-map
  "The options to supply to each component when it's constructed"
  {s/Keyword s/Any})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internals

(s/defn initialize :- [[(s/one s/Keyword "name") (s/one s/Any "instance")]]
  "require the individual namespaces and call each Component's constructor,
returning a seq of name/instance pairs that probably should have been a map

N.B. this returns key-value pairs that are suitable for passing to dependencies
as the last argument of apply"
  [descr :- {s/Keyword (s/either s/Symbol [(s/one s/Symbol "name") s/Any])}
   config-options :- {s/Any s/Any}]
  (mapcat (fn [[name ctor]]
            ;; If the config file needs parameters, it can
            ;; specify the "value" of each component as
            ;; a sequence
            (let [[ctor-sym args] (if (symbol? ctor)
                                    [ctor [{}]]  ; no args supplied
                                    [(first ctor) (rest ctor)])]
              ;; Called for the side-effects
              (-> ctor-sym namespace symbol require)
              (let [real-ctor (resolve ctor-sym)
                    instance (apply real-ctor args)]
                [name instance])))
          descr))

(s/defn ^:always-validate system-map :- SystemMap
  [descr :- system-structure
   config-options :- {s/Any s/Any}]
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
