(ns component-dsl.context-manager-test
  "Verify that the context-manager works
and that we can use it to signal the done-manager
that it's time to close up shop and go home for the
night"
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
                                (try
                                  (-> everything :waiter :done (deliver "woohoo!"))
                                  (catch Exception ex
                                    (println "*******************************
!!!!!!!!!!!!Danger Will Robinson!!!!!!!!!
*****************************")))))]
      ;; Actually create the context where everything will/should run
      (ctx/setup everything)
      ;; This will wait for the done manager forever
      (ctx/context (fn [system]
                     (->> system :waiter :done deref
                          (is "woohoo!")))))))

(deftest check-with
  (let [everything (sys/build {:structure (waiter)
                               :dependencies {}}
                              {})]
    (try
      ;; Actually create the context where everything will/should run
      (ctx/setup everything)
      (let [n 3]
        (ctx/with-component :waiter
          [waiter-component n]
          (testing "Validate basic with-component macro"
            (is (= (keys waiter-component) [:done])))
          (testing "Parameter passing"
            (is (= n 3))))))))
