(ns component-dsl.system-test
  "Make sure basic assumptions re: System descriptions work"
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
    :routes component-dsl.routes/ctor})

(s/defn initialize-web
  "Returns the web portion of the System defined by simple-web-components"
  []
  ;; This is really pretty cheap and pedantic
  (let [description (simple-web-components)]
    (first (sys/initialize description {}))))

(s/defn system-with-dependencies :- com.stuartsierra.component.SystemMap
  "Yes, this is copy/pasted from the verify-dependencies test"
  []
  (let [initialized (sys/system-map (simple-web-components) {})
        dependency-description {:web [:routes]}]
    (sys/dependencies initialized dependency-description)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests

(deftest parameter-creation
  "Working out what initialize-web actually needs to do.
This test is ugly and digs into far too many low-level details.
It should probably just go away."
  []
  (let [descr '{:web component-dsl.core/ctor
                :routes component-dsl.routes/ctor}
        creator (fn [[name ctor]]
                  (println "Creating" name "using" ctor)
                  ;; Called for side-effects so we have
                  ;; a constructor to call
                  ;; Q: Do we actually need this?
                  (-> ctor namespace symbol require)
                  (let [real-ctor (resolve ctor)]
                    (let [instance (real-ctor {})]
                      (comment (println "instance:" instance))
                      [name instance])))
        parameters (mapcat creator descr)]
    (is (first parameters) :web)
    (is (nth parameters 2) :routes)
    ;; Because I was having issues with nil before
    ;; I tweaked resolve to actually turn the ctor
    ;; into a function to call
    (let [web-server (second parameters)]
      (is web-server)
      ;; This is printing oddly. I think it's just
      ;; nil values inside the map, but it's printing
      ;; as though the values just don't exist
      (let [routes (get web-server :routes :not-found)]
        ;; No routes until we've started...
        ;; but the key is there
        (is (= :not-found routes))
        ;; I really expected this to return either nil or false
        ;; The key's there. Or, at least, it's getting printed.
        ;; TODO: Dig deeper into the guts to figure out why I
        ;; don't have what I expect
        (is (= (get web-server :started :not-started) :not-started))))
    (is (nth parameters 3))))

(deftest initialization
  "Verify that we can describe and call a basic constructor"
  []
  (let [initialized (initialize-web)]
    (is (= :web (first initialized)))
    (is (= {:port 8000, :resource-root "www"} (nth initialized 1)))))

(deftest verify-dependencies
  "Does the dependency map work the way I think?"
  []
  (let [initialized (sys/system-map (simple-web-components) {})
        dependency-description {:web [:routes]}]
    (let [dependency-tree (sys/dependencies initialized dependency-description)]
      ;; That really just modifies the metadata
      (is (= dependency-tree initialized))
      (is (= {:com.stuartsierra.component/dependencies {:routes :routes}}
             (-> dependency-tree :web meta)))
      (is (nil? (-> dependency-tree :routes meta))))))

(comment
  (deftest start-stop
    "System's pretty useless if it can't do this.
But it's based on a real web server that I'm using
in the project from which this was refactored"
    []
    (let [dependency-tree (system-with-dependencies)]
      (is (-> dependency-tree :web))
      (is (not (-> dependency-tree :web :started-options)))
      (is (not (-> dependency-tree :web (get :started-options :not-found))))
      (let [started (component/start dependency-tree)]
        (try
          (is (-> started :web :started-options))
          (is (= (-> started :routes keys) [:handlers]))
          ;; TODO: Is it worth checking for anything else?
          (finally
            (component/stop started)))))))

(comment
  (let [inited (sys/system-map (simple-web-components) {:web :server
                                                        :routes {:handler :route-description}})
        system (sys/dependencies inited {:web [:routes]})
        started (component/start system)]
    (-> started keys)))

(deftest almost-realistic
  "This is getting closer to something you might actually do"
  []
  (let [initialized (sys/system-map (simple-web-components) {})
        system (sys/dependencies initialized {:web [:routes]})
        started (component/start system)]
    ;; This is where all your work should happen
    (is (= (:routes started)
           (-> started :web :routes))
        "System started successfully")
    (component/stop started)
    (is true "Managed to stop without error")))

(deftest really-close
  "For most cases, this approach probably makes the most sense"
  []
  (let [descr {:structure (simple-web-components)
               :dependencies {:web [:routes]}}
        inited (sys/build descr {})
        started (component/start inited)]
    (is (= (:routes started)
           (-> started :web :routes))
        "System started successfully")
    (is (= (-> started :web :port) 8000)
        "Set up default port correctly")
    (component/stop started)))

(deftest realistic
  "How you probably want to use it"
  []
  (let [world (-> "sample.system.edn" sys/ctor component/start)]
    (try
      (is (= (:routes world)
             (-> world :web :routes))
          "System properly configured")
      (catch RuntimeException ex
        (is false "How'd an exception get thrown?"))
      (finally
        (component/stop world)))
    (is true "Managed to stop without error")))

(deftest schema-extractors
  []
  (testing "Minimalist schema loading"
    (let [descr {'one '[schema-a schema-b]
                 'two 'schema-a}]
      (sys/require-schematic-namespaces! descr)
      (is (= (sys/load-var 'one 'schema-a)
             {:a s/Int, :b s/Int}))
      (testing "Schema description merge"
        ;; The order in which these are returned really is
        ;; just an implementation detail
        ;; For now, this approach makes the test quite a bit simpler
        (is (= (sys/extract-schema descr)
               [{:a s/Int, :b s/Int}
                {:z s/Str, :y s/Keyword}
                {:a s/Str :z s/Int}]))))))

;;; TODO: Need test(s) for supplying constructor options
(deftest construct-with-parameters
  []
  (testing "Supply parameters to constructors"
    (let [descr {:structure (simple-web-components)
                 :dependencies {:web [:routes]}}
          inited (sys/build descr {:web {:port 9010
                                         :resource-root "public"}})
          started (component/start inited)]
      (is (= (-> started :web :port)
             9010)
          "Assigned port successfully")
      (is (= (-> started :web :resource-root)
             "public")
          "Assigned web root successfully")
      (component/stop started))))
