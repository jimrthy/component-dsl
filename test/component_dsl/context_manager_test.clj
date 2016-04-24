(ns component-dsl.context-manager-test
  (:require [clojure.test :refer [deftest is testing] :as test]
            [component-dsl.context-manager :as ctx]
            [component-dsl.system :as sys]))

(defn waiter
  "System that consists of a flag to exit"
  []
  '{:waiter component-dsl.done-manager/ctor})

(deftest synchronous-run
  (testing "Wait for a system to run"
    (let [everything (sys/build {:structure (waiter)
                                 :dependencies {}}
                                {})
          closer (future-call (fn []
                                ;; Surely 10 ms is an eternity for this
                                ;; kind of test
                                (Thread/sleep 10)
                                (-> everything :done :done (deliver "woohoo!"))))]
      ;; Actually create the context where everything will/should run
      (ctx/setup everything)
      ;; This will wait for the done manager forever
      (ctx/context (fn [system]
                     (->> everything :done :done deref
                          (is "woohoo!")))))))
