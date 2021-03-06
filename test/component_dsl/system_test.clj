(ns component-dsl.system-test
  "Make sure basic assumptions re: System descriptions work

TODO: Rename all these tests to .cljc"
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clojure.test :refer [deftest is testing] :as test]
            [com.stuartsierra.component :as component]
            [component-dsl.system :as sys]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defn simple-web-components
  []
  '{:web component-dsl.core/ctor
    :routes component-dsl.routes/ctor})

(defn initialize
  []
  (let [description (simple-web-components)]
    (sys/initialize description {})))

(defn initialize-web
  "Returns the web portion of the System defined by simple-web-components"
  []
  ;; This is really pretty cheap and pedantic
  (first (initialize)))

(s/fdef system-with-dependencies
        :ret :component-dsl.system/system-map)
(defn system-with-dependencies
  "Yes, this is copy/pasted from the verify-dependencies test"
  []
  (let [initialized (sys/system-map (simple-web-components) {})
        dependency-description {:web [:routes]}]
    (sys/dependencies initialized dependency-description)))

(defn nested-components
  []
  (let [descr {:component-dsl.system/structure '{::database component-dsl.example-db/ctor
                                                 ::schema component-dsl.example-db/schema-builder}
               :component-dsl.system/dependencies {::schema {:db ::database}}}
        options {::database {:url "http://database:2020/connection"}
                 ::schema {:definition "http://database/schema.html"}}
        basic-structure (simple-web-components)
        nested-wrapper #:component-dsl.system{:system-configuration descr
                                              :configuration-tree options
                                              :primary-component ::database}]
    {:description {:component-dsl.system/structure (assoc basic-structure
                                                          :nested nested-wrapper)
                   :component-dsl.system/dependencies {:web [:routes]
                                                       :routes [:nested]}}
     :options {:web {:port 2600
                     :resource-route "/home/www/public/"}
               :routes {:handler (fn [_]
                                   {:code 200
                                    :body "Hello world"})}}}))

(s/fdef nested-struct-builder
        :args (s/cat :opts (s/keys :opt [::schema-definition ::url]))
        :ret :component-dsl.system/nested-definition)
(defn nested-struct-builder
  [{:keys [::schema-definition
           ::url]
    :or {schema-definition "http://database/schema.html"
         url "http://database:2020/connection"}
    :as opts}]
  (let [nested-struct '{::database component-dsl.example-db/ctor
                        ::schema component-dsl.example-db/schema-builder}
        nested-deps {::schema {:db ::database}}
        nested-opts {::database {:url url}
                     ::schema {:definition schema-definition}}]
    #:component-dsl.system{:system-configuration #:component-dsl.system{:structure nested-struct
                                                                        :dependencies nested-deps}
                           :configuration-tree nested-opts
                           :primary-component ::database}))

(defn hard-coded-nested-structure
  []
  (let [nested (nested-struct-builder {})
        description `#:component-dsl.system{:structure {:web component-dsl.core/ctor,
                                                        :routes component-dsl.routes/ctor,
                                                        :nested ~nested},
                                            :dependencies {:web [:routes],
                                                           :routes [:nested]}}
        options {:routes {:handler (fn [_]
                                     {:code 200
                                      :body "Hello world"})}
                 :web {:port 2600, :resource-route "/home/www/public/"},}]
    ;; Note that these are really just used internally.
    ;; They get passed to build and pre-process as 2 separate arguments.
    ;; Though that's starting to seem a little silly
    {::description description
     ::options options}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests

(deftest check-dependency-value-translation
  (testing "sequential"
    (let [dependencies [:b :c :d]]
      (is (= {:b :b,
              :c :replacement,
              :d :d}
             (sys/translate-dependency-value dependencies :c :replacement)))))
  (testing "associative"
    (let [dependencies {:c-cpt :c, :d-cpt :d, :e-cpt :e}]
      (is (= {:c-cpt :replacement
                  :d-cpt :d
              :e-cpt :e}
             (sys/translate-dependency-value dependencies :c :replacement))))))
(comment (check-dependency-value-translation))

(deftest check-nested-dependency-resolution
  ;;; This is actually a pretty low-level implementation-specific test.
  ;;; But I had to start somewhere
  (testing "Dependencies of/on a nested Component flatten correctly"
    ;; The nested structural definitions really need to be namespaced
    ;; But we can't feed them into records that way.
    (let [nested-dependencies {:nested/schema {:database :nested/database}}
          nested '#:sys{:system-configuration #:sys{:structure {:nested/database component-dsl.example-db/ctor,
                                                                :nested/schema component-dsl.example-db/schema-builder},

                                                    :dependencies nested-dependencies},
                        :configuration-tree {:nested/database {:url "http://database:2020/connection"},
                                             :nested/schema {:definition "http://database/schema.html"}},
                        :primary-component :nested/database}
          dependencies {:depends-on [:nested :unrelated]
                        :nested {:url :location}
                        :unrelated [:in-parent]}]
      (let [pre-merged (into dependencies (dissoc nested-dependencies :nested/database))
            merged (sys/resolve-nested-dependencies pre-merged :nested :nested/database)]
        (testing "structural merge"
          (is (= {:nested :nested/database, :unrelated :unrelated} (:depends-on merged)))
          (is (= {:url :location} (:nested/database merged)))
          (is (= {:in-parent :in-parent} (:unrelated merged))))
        ;; This part is over-simplified.
        ;; It really happens in the middle of a reduce, so the empty
        ;; hashmap I'm using as an accumulator isn't realistic except as
        ;; the simplest edge case.
        ;; Then again, I had to start somewhere.
        (let [resolved (sys/merge-dependency-trees {} merged)]
          (is (= {:depends-on {:nested :nested/database
                               :unrelated :unrelated}
                  :nested/database {:url :location}
                  :nested/schema {:database :nested/database}
                  :unrelated {:in-parent :in-parent}}
                 resolved)))))))
(comment (check-nested-dependency-resolution))

(deftest check-manual-build-steps
  (let [{:keys [::description ::options]} (hard-coded-nested-structure)]
    (testing "Individual steps that happen during build"
      (try
        (let [pre-processed (sys/pre-process (assoc description ::options options))
              _ (is pre-processed)
              {:keys [:component-dsl.system/dependencies
                      :component-dsl.system/structure
                      :component-dsl.system/options]}  pre-processed
              _ (is dependencies (str "Missing dependencies in\n"
                                      (with-out-str (pprint pre-processed))
                                      "with keys\n\t" (keys pre-processed)))
              _ (is structure (str "Missing ::structure in\n"
                                   (with-out-str (pprint pre-processed))
                                   "with keys\n\t" (keys pre-processed)))
              ;; Note that options has been pre-processed now
              pre-init (sys/system-map structure options)
              created (sys/dependencies pre-init dependencies)
              started (component/start created)]
          (try
            ;; Q: What about a predicate to ensure that each entry has exactly one match?
            ;; Q: Is there a good short-hand for specifying the namespace on the keywords to
            ;; this set?
            ;; Working with this at the REPL doesn't seem like it's going to be a lot of fun.
            ;; Maybe I don't want to mess w/ trying to keyword these at al.
            (is (= #{:web :routes
                     ::database ::schema}
                   (set (keys started))))
            (let [actual-db (::database started)]
              (is actual-db)
              (is (= actual-db (-> started :routes :nested))
                  (str "Database assigned to routes does not match\n"
                       "Available keys: "
                       (keys (:routes started))))
              ;; This is getting translated to :nested, even though it definitely shouldn't be.
              ;; I'm also getting an extra key that's an empty hash-map
              (is (= actual-db (-> started ::schema :db))
                  (str "Database assigned to schema does not match\n"
                       "schema:\n"
                       (with-out-str (pprint (::schema started))))))
            (finally (component/stop started))))
        (catch clojure.lang.ExceptionInfo ex
          (println "Failed:" (.getMessage ex))
          (let [failure-details (.getData ex)]
            (println "Problem overview:\n" (keys failure-details))
            (pprint failure-details)
            (.printStackTrace ex)
            (is (not ex))))))))
(comment (check-manual-build-steps)
         (let [{:keys [::description ::options]} (hard-coded-nested-structure)]
           (let [x
                 (->> (sys/pre-process description options)
                      :component-dsl.system/options
                      #_keys
                      #_:component-dsl.system/structure
                      #_:component-dsl.system/dependencies
                      )]
             (println "\n\nPre-processed:")
             (pprint x)))
         )

(deftest check-manually-nested-components
  (testing "Q: Why would anyone ever want to do this?
A: They wouldn't.
This is just proving out the concept that's hopefully useful in check-nested-components
(where this sort of System definition is built incrementally)"
    (println "***********************************************************
Starting manual nesting test
***********************************************************")
    (let [{:keys [::description ::options]} (hard-coded-nested-structure)]
      (try
        (let [created (sys/build description options)
              started (component/start created)]
          (try
            ;; Q: What about a predicate to ensure that each entry has exactly one match?
            ;; Q: Is there a good short-hand for specifying the namespace on the keywords to
            ;; this set?
            ;; Working with this at the REPL doesn't seem like it's going to be a lot of fun.
            ;; Maybe I don't want to mess w/ trying to keyword these at al.
            (is (= #{:web :routes
                     ::database ::schema}
                   (set (keys started))))
            (let [actual-db (::database started)]
              (is actual-db)
              (is (= actual-db (-> started :routes :nested)))
              (is (= actual-db (-> started ::schema :db))))
            (finally
              ;; Not really needed for this test, but it's a really good
              ;; general habit
              (component/stop started))))
        (catch clojure.lang.ExceptionInfo ex
          (println "Failed:" (.getMessage ex))
          (let [failure-details (.getData ex)]
            (println "Problem overview:\n" (keys failure-details))
            (pprint failure-details)
            (.printStackTrace ex)
            (is (not ex))))))))
(comment (check-manually-nested-components))

(deftest nested-builder
  (let [struct #:component-dsl.system{:structure '{:nested component-dsl.system-test/nested-struct-builder
                                                   :web component-dsl.core/ctor
                                                   :routes component-dsl.routes/ctor}
                                      :dependencies {:web [:routes],
                                                     :routes [:nested]}}
        options {:web {:port 2600, :resource-route "/home/www/public/"},
                 :routes {:handler (fn [_]
                                     {:code 200
                                      :body "Hello world"})}
                 :nested {::url "http://database:32020/override"}}]
    (testing "Use a struct builder as a nested ctor"
      (try
        (let [created (sys/build struct options)
              started (component/start created)]
          (try
            (is (= "http://database:32020/override" (-> started ::database :url)))
            (finally (component/stop started))))
        (catch clojure.lang.ExceptionInfo ex
          (let [data (.getData ex)]
            (is false (str "nested-builder initialization failed\n"
                           (.getMessage ex) "\n"
                           (with-out-str (pprint data))
                           (if-let [^Exception cause (:cause data)]
                             (str "Nested root cause: " (.getMessage cause))
                             "Cause not specified")))))))))
(comment (nested-builder)
         )

;;; This should really be a duplicate of check-manually-nested-components.
;;; I'm just being more "clever" about building the actual component tree
;;; definitions, in an attempt to avoid duplicate code.
(deftest check-nested-components
  (let [descr (nested-components)]
    (try
      (let [created (sys/build (:description descr) (:options descr))
            started (component/start created)]
        (try
          ;; Q: What about a predicate to ensure that each entry has a match?
          (is (= #{:web :routes ::database ::schema} (set (keys created))))
          (let [actual-db (::database created)]
            (is actual-db)
            (is (= actual-db (-> started :routes :nested)))
            (is (= actual-db (-> started ::schema :db))))
          (finally
            (component/stop created))))
      (catch clojure.lang.ExceptionInfo ex
        (println "Failed:" (.getMessage ex))
        (let [failure-details (.getData ex)]
          (println "Problem overview:\n" (keys failure-details))
          (pprint failure-details)
          (.printStackTrace ex)
          (is (not ex)))))))
(comment
  (pprint (nested-components))
  (let [descr (nested-components)]
    (comment) (try
                (let [created (sys/build (:description descr) (:options descr))]
                  #_(try
                    (let [started (component/start created)]
                      (try
                        ;; Q: What about a predicate to ensure that each entry has a match?
                        ;; This would almost be an inversion of every?.
                        ;; Surely there's a core function for this.
                        (keys started)
                        (-> descr :description :component-dsl.system/dependencies)
                        (-> started :routes keys)
                        (catch clojure.lang.ExceptionInfo ex
                          (println "Failed after building")
                          (println (.getMessage ex))
                          (pprint (.getData ex)))))
                    (catch clojure.lang.ExceptionInfo ex
                      (println "Failed during creation")
                      (println (.getMessage ex))
                      (pprint (.getData ex))
                      (.printStackTrace ex)))
                  (-> created ::schema meta))
                (catch clojure.lang.ExceptionInfo ex
                  (println "Failed during creation")
                  (println (.getMessage ex))
                  (pprint (.getData ex))
                  (.printStackTrace ex)))
    #_(keys (:description descr)))
  )

(deftest check-configuration-override
  ;; Outer layers should be able to override the configuration specified
  ;; by the inner.
  ;; This seems backwards, but we're working our way from the low-level specific
  ;; details at the core to the high-level components at the outside that an
  ;; app will actually interact with
  (let [raw-descr (nested-components)
        url-override "tcp://database:2020/prod-connection"
        descr (assoc-in raw-descr [:options ::database :url] url-override)
        built (sys/build (:description descr) (:options descr))]
    (is built)
    (is (= url-override (-> built ::database :url)))))
(comment
  (check-configuration-override)
  (let [raw-descr (nested-components)
        url-override "tcp://database:2020/prod-connection"
        descr (assoc-in raw-descr [:options ::database :url] url-override)
        built (sys/build (:description descr) (:options descr))]
    (-> built keys))
  )

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
  (let [descr {:component-dsl.system/structure (simple-web-components)
               :component-dsl.system/dependencies {:web [:routes]}}
        inited (sys/build descr {})
        started (component/start inited)]
    (try
      (is (= (:routes started)
             (-> started :web :routes))
          "System started successfully")
      (is (= 8000 (-> started :web :port))
          "Set up default port correctly")
      (finally
        (component/stop started)))))
(comment
  (let [descr #:component-dsl.system{:structure (simple-web-components)
                                     :dependencies {:web [:routes]}}
        inited (sys/build descr {})]
    inited
    (sys/pre-process descr))
  )

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
    (let [descr '{one [for-checking-var-extraction also]
                  two for-checking-var-extraction}]
      (sys/require-schematic-namespaces! descr)
      ;; TODO: Still need a test for load-var
      (comment
        (is (= (sys/load-var 'one :schema-a)
               {:a s/Int, :b s/Int})))
      (let [x {:one/a 1 :one/b 2}]
        (is (= (s/conform :one/schema-a x) x))
        ;; Note that this just silently ignores the extra key
        (let [extended (assoc x :one/c "bad")]
          (is (= extended
               (s/conform :one/schema-a extended))))
        (is (= (s/conform :one/schema-a (dissoc x :one/b))
               :clojure.spec/invalid)))
      (testing "Schema description merge"
        (is (every? #{"from ns one"
                      "also from ns one"
                      "from ns two"}
                    (sys/extract-var-values descr)))))))

(comment
  (require '[one])
  (let [x {:one/a 1 :one/b 2 :one/c "bad"}]
    (s/conform :one/schema-a x))
  )

;;; TODO: Need test(s) for supplying constructor options
(deftest construct-with-parameters
  []
  (testing "Supply parameters to constructors"
    (let [descr {:component-dsl.system/structure (simple-web-components)
                 :component-dsl.system/dependencies {:web [:routes]}}
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
