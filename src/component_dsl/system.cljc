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

(s/def ::nested-definition (s/keys :req {::system-configuration ::initialization-map
                                         ::configuration-tree ::configuration-tree}))

;; When a Component's dependency name doesn't match what the System calls it
;; Key is what your Component calls it, value is what the System does
(s/def ::component-name-map (s/map-of ::component-name ::component-name))

(s/def ::component-ctor-id symbol?)

;;; Component 'name' to the namespaced initialization function
(s/def ::initialization-map (s/map-of ::component-name (s/or :ctor ::component-ctor-id
                                                             :nested ::nested-definition)))
(s/def ::flattened-initialization-map (s/map-of ::component-name ::component-ctor-id))

(s/def ::dependency-seq (s/or :seq ::component-name
                              :map ::component-name-map))
;; Component 'name' to a seq of the 'names' of its dependencies
(s/def ::system-dependencies (s/map-of ::component-name ::dependency-seq))

;; Describe the system structure and its dependencies
(s/def ::flattened-description (s/keys ::req {::structure ::flattened-initialization-map  ; this is a poor naming choice
                                              ;; Very tempting to make this optional
                                              ;; It won't be, except for trivial systems.
                                              ;; But it's annoying to remember for those.
                                              ;; Ah, well. If you're writing something trivial, don't use this.
                                              ::dependencies ::system-dependencies}))

(s/def ::system-description (s/keys ::req {::structure ::initialization-map
                                           ::dependencies ::system-dependencies}))

;; key-value pairs that are suitable for passing to dependencies
;; as the last argument of apply.
;; TODO: I think I actually want s/alt
(s/def ::named-instances (s/cat :name keyword?
                                :instance any?))

(s/def ::name-space symbol?)
(s/def ::schema-name keyword?)

;; A spec defining...something.
;; TODO: Is there schema for describing legal specs anywhere?
;; Q: Where is this actually used?
(s/def ::spec any?)

;; Really just so I have a meaningful name to call these things
(s/def ::specs
  (s/coll-of ::spec))

;; Really just a map of symbols marking a namespace to
;; keywords naming interesting vars in that namespace
(s/def ::schema-description
  (s/map-of ::name-space (s/or :single ::schema-name
                               :seq (s/coll-of ::schema-name))))

;; TODO: Try redefining ComponentName as this
;; I'm just not sure that'll work in non-sequences
;; (such as when I'm using it as the key in a map)
(s/def ::component-instance-name
  keyword?)
(s/def ::component-instance map?)
;; This is really a pair of the keyword "name" and the unstarted instance
(s/def ::component (s/cat ::component-instance-name ::component-instance))

;; Q: Can I get anything more interesting from this?
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
  "Makes sure all the namespaces where the specs
are found are available, so we can access the specs"
  [description]
  (dorun (map #?(:clj require)
              #?(:cljs (throw (ex-info "This exists now; just need the will to use it" {:missing %})))
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
        :ret ::specs)
(defn extract-var-values
  "Returns a seq of the values of the vars in each namespace"
  [descriptions]
  (mapcat (fn [[namespace var-name]]
            (if (symbol? var-name)
              [(load-var namespace var-name)]
              (map (partial load-var namespace) var-name)))
          descriptions))

(s/fdef translate-schematics!
        :args (s/cat :description ::schema-description)
        :ret ::specs)
(defn translate-schematics!
  "require the namespace and load the vars specified in each.

  N.B. Doesn't even think about trying to be java friendly. No defrecord!

  Q: Is there any point to this?"
  [description]
  (require-schematic-namespaces! description)
  (extract-var-values description))

(s/fdef build-instances
        :args (s/cat :name keyword?
                     :ctor (s/fspec :args (s/cat :options map?)
                                    :ret (s/coll-of ::component))))
(declare initialize)
(defn build-instances
  [name ctor local-options]
  (let [instance
        (try (ctor local-options)
             (catch #?(:clj NullPointerException)
                 #?(:cljs :default)  ; Q: What about other platforms?
                 ex
                 (let [msg (str ex
                                "\nTrying to call ctor for "
                                name
                                "\nwith params:\n"
                                local-options
                                "\nHonestly, this is fatal")]
                   (throw (#?(:clj RuntimeException.) #?(:cljs js/Error.) msg ex)))))]
    (if (s/valid? ::nested-definition instance)
      (do
        (println "Recursing based on" (keys instance))
        (throw (ex-info "Should have already handled recursion")))
      (println "Won't recurse because" name "is at bottom"))
    (condp s/valid? instance
      ;; recursion FTW!
      ::nested-definition (initialize (::description instance) (::options instance))
      map? [[name instance]])))

(s/fdef create-individual-component
        :args (s/cat :config-options ::configuration-tree
                     :description (s/cat :name ::component-name
                                         :ctor-sym ::component-ctor-id)))
(defn create-individual-component
  [config-options [name ctor-sym]]
  ;; Called for the side-effects
  ;; This should have been taken care of below,
  ;; in the call to require-schematic-namespaces!
  ;; But it didn't work.
  ;; TODO: Make sure require-schematic-namespaces! gets called
  ;; (and works)
  ;; for this step. Really
  ;; don't want to put this sort of side-effect in
  ;; the middle of a big nasty function like this.
  (try
    (assert ctor-sym)
    (let [ctor-ns (namespace ctor-sym)
          _ (assert ctor-ns (str "No namespace associated with " ctor-sym " for Component " name))
          ns-sym (symbol ctor-ns)]
      (when (= ns-sym 'component-dsl.system)
        (throw (ex-info "Trying to create component from system ns"
                        {:problem "Causes a recursive require"
                         :name name
                         :ctor-sym ctor-sym
                         :ns-sym ns-sym})))
      (#?(:clj require) #?(:cljs (throw (ex-info "That exists. Use it" {}))) ns-sym))
    (catch ClassCastException ex
      (throw (ex-info "Failed to require the associated namespace symbol"
                      {:ctor-sym ctor-sym
                       :component-name name
                       :config-options config-options}))))
  (if-let [ctor (#?(:clj resolve)
                 #?(:cljs
                    (throw (ex-info "This can't possibly work as-is...what's the equivalent?" {})))
                 ctor-sym)]
    (let [
          ;; Don't force caller to remember this;
          ;; even though it really is a required
          ;; part of the protocol/API. Leaving it
          ;; off is just begging for errors
          local-options (get config-options name {})]
      (build-instances name ctor local-options))
    (throw (#?(:clj RuntimeException.) #?(:cljs js/Error.) (str "No such constructor:\n"
                                                                ctor-sym)))))

(s/fdef create-nested-components
        :args (s/cat :config-options ::configuration-tree
                     :nested ::nested-definition)
        ;; Seem like it would be silly for a nested definition to just return
        ;; one thing.
        ;; But this could totally happen if the structure was generated
        ;; programmatically.
        :ret (s/or :single ::component-instance
                   :multiple (s/coll-of ::component-instance)))
(defn create-nested-components
  [config-options nested]
  (initialize (-> nested ::system-configuration ::structure)
              (-> nested ::configuration-tree)))

(s/fdef create-component
        :args (s/cat :config-options ::configuration-tree
                     :description (s/cat :name keyword?
                                         :description (s/or :ctor-sym symbol?
                                                            :nested ::nested-definition)))
        :ret (s/or :individual ::component-instance
                   :multiples (s/coll-of ::component-instance)))
(defn create-component
  [config-options definition]
  ;; Would really be better to just supply the ctor as a function.
  ;; This approach is a hold-over from the time when I liked the idea of
  ;; just loading the definitions from EDN.
  ;; It's really too macro-ish
  (let [[name ctor-descr] definition]
    (println "cpt-dsl.system/create-component: Trying to create component" name "using the constructor" ctor-descr)
    (if (symbol? ctor-descr)
      (create-individual-component config-options definition)
      (create-nested-components config-options ctor-descr))))

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
        (mapcat (partial create-component config-options)
             descr)]
    (comment) (println "cpt-dsl.system/initialize Initialized System:\n"
                       (with-out-str (pprint result)))
    result))

;; TODO: This is another to always validate
(s/fdef system-map
        :args (s/cat :descr ::initialization-map
                     :config-options ::configuration-tree)
        :ret ::system-map)
(defn system-map
  [descr config-options]
  (println "cpt-dsl.system/system-map -- Initializing system\n"
           (with-out-str (pprint descr))
           "with options:\n"
           (with-out-str (pprint config-options)))
  (let [inited-pairs (initialize descr config-options)
        inited (apply concat inited-pairs)
        result (apply component/system-map inited)]
    (println "cpt-dsl.system/system-map:\nInited-pairs:\n"
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

(s/fdef merge-dependencies
        :args (s/cat :template ::dependency-seq
                     :realized ::dependency-seq)
        :ret ::dependency-seq)
(defn merge-dependencies
  [template realized]
  (throw (ex-info "Start Here" {})))

(s/fdef pre-process
        :args (s/cat :description ::system-description)
        :ret ::flattened-description)
(defn pre-process
  "Have to flatten out a nested system definition"
  [{:keys [::structure ::dependencies]}]
  (let [mod-deps-atom (atom dependencies)
        flattened-structure (reduce (fn [acc [name ctor]]
                                      (if (symbol? ctor)
                                        (do
                                          ;; Baseline unnested case
                                          (assert (not (contains? acc name)) (str "Duplicate key specified: " name
                                                                                  "\nExisting: " (get acc name)
                                                                                  "\nReplacement: " ctor))
                                          (assoc acc name ctor))
                                        (let [{:keys [::structure ::dependencies]
                                               :as de-nested} (pre-process ctor)]
                                          (doseq [k (keys structure)]
                                            (assert (not (contains? acc k)) (str "Nested " k "\n - " (get structure k)
                                                                                 "\ntrying to replace existing constructor " (get acc k))))
                                          (swap! mod-deps-atom resolve-nested-dependencies ctor)
                                          (into acc structure))))
                                    {}
                                    structure)]
    {::structure flattened-structure
     ::dependencies @mod-deps-atom}))

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
  (println "Building a system from keys" (keys descr))
  (let [pre-processed (pre-process descr)
        pre-init (system-map (::structure pre-processed) options)]
    (dependencies pre-init (::dependencies pre-processed))))

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
and more dubious as I keep moving forward and ignoring it in favor of
build instead.

This approach is more flexible at runtime, in terms of mixing/matching
Components, but it seemed less responsive at build time. Should probably
try it again now that I'm revisiting this pretty seriously."
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
