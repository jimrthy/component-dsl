(ns component-dsl.renamable-dependency-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is testing] :as test]
            [com.stuartsierra.component :as component]
            [component-dsl.system :as sys]
            [schema.core :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defn simple-web-components
  []
  '{:web component-dsl.core/ctor
    :route-manager component-dsl.routes/ctor})

(s/defn system-with-dependencies :- com.stuartsierra.component.SystemMap
  "Yes, this is copy/pasted from the verify-dependencies test"
  []
  (let [initialized (sys/system-map (simple-web-components) {})
        dependency-description {:web {:routes :route-manager}}]
    (sys/dependencies initialized dependency-description)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests

(deftest rename-dependency
  "Make sure dependency renaming works the way I think"
  []
  (let [system (system-with-dependencies)
        started (component/start system)
        routes (:route-manager started)
        web (:web started)]
    (is (= routes (:routes web)))))
