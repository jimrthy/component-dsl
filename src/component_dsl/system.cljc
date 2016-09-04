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

(s/def ::component-name simple-keyword?)

;; The options to supply to each component when it's constructed
;; For flexibility, everything should be legal here.
;; But, come on, when would the keys ever be anything except
;; keywords?
;; Note that the keys are not ::component-name!
;; That's a higher-level abstraction captured below in
;; ::configuration-tree
(s/def ::option-map (s/map-of keyword? any?))

;;; Which parameters get passed to the various constructors?
;;;
;;; The keys are the corresponding names of each Component.
;;;
;;; This seems like a silly approach...why not just describe
;;; each Component, include its constructor and associated args,
;;; and its dependencies altogether?
;;;
;;; The entire point is really to make all those details
;;; declarative and easy, then to merge them into that sort
;;; of single map in here.
(s/def ::configuration-tree (s/map-of ::component-name ::option-map))

(s/def ::nested-definition (s/keys :req [::system-configuration
                                         ::configuration-tree
                                         ::primary-component]
                                   #_{::system-configuration ::initialization-map
                                      ::configuration-tree ::configuration-tree
                                      ::primary-component ::component-name}))

;; When a Component's dependency name doesn't match what the System calls it
;; Key is what your Component calls it, value is what the System does
(s/def ::component-name-map (s/map-of ::component-name ::component-name))

(s/def ::component-ctor-id symbol?)

;;; Map of Component 'name' => (or namespaced-initialization-function nested-definition)
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
    (when (s/valid? ::nested-definition instance)
      (do
        (println "Recursing because\n"
                 (with-out-str (pprint instance))
                 "\ncreated by calling" ctor
                 "\non" (with-out-str (pprint local-options))
                 "\nfor the Component named" name
                 "\nis a valid ::nested-definition"
                 "\nConformed version looks like:\n"
                 (with-out-str (pprint (s/conform ::nested-definition instance))))
        (throw (ex-info "Should have already handled recursion" {}))))
    [[name instance]]))

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

(s/fdef override-options
        :args (s/cat :defaults ::configuration-tree
                     :overrides ::configuration-tree)
        :ret ::configuration-tree)
(defn override-options
  [defaults overrides]
  (let [msg (str "Trying to override\n"
                 (with-out-str (pprint defaults))
                 "\nwith\n"
                 (with-out-str (pprint overrides)))]
    (throw (ex-info msg {:problem "Not Implemented"
                         :defaults defaults
                         :overrides overrides}))))

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
  (println (str "component-dsl.system/created-nested-components\n\tOptions:"
                (with-out-str (pprint config-options))
                "\tnested definitions:\n"
                (with-out-str (pprint nested))))
  (let [default-options (::configuration-tree nested)]
    (initialize (-> nested ::system-configuration ::structure)
                (override-options default-options config-options))))

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
    (println "cpt-dsl.system/create-component: Trying to create component" name
             "using the constructor" ctor-descr)
    (if (symbol? ctor-descr)
      (create-individual-component config-options definition)
      (create-nested-components config-options ctor-descr))))

;;; Q: What's the spec equivalent of schema's ^:always-validate?
(s/fdef initialize
        :args (s/cat :descr ::initialization-map
                     :config-options ::configuration-tree)
        :ret (s/coll-of ::component))
(defn initialize
  "require the individual namespaces and call each Component's constructor.
Returns a seq of name/instance pairs that probably should have been a map."
  [descr config-options]
  (println (str "component-dsl.system/initialize\n\tStructure:\n"
                (with-out-str (pprint descr))
                "\tOptions:\n"
                (with-out-str (pprint config-options))))
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

(s/fdef translate-dependency-value
        :args (s/cat :val ::dependency-seq
                     :to-replace ::component-name
                     :replacement ::component-name)
        :ret ::dependency-seq)
(defn translate-dependency-value
  [val to-replace replacement]
  (reduce (fn [acc [k v]]
            (let [v' (if (not= v to-replace)
                       v replacement)]
              (conj acc [k v'])))
          {}
          (if (sequential? val)
            ;; Surely there's already a built-in for this
            (reduce (fn [acc x]
                      (assoc acc x x))
                    {} val)
            val)))
(comment
  (translate-dependency-value [:b :c :d] :c :replacement)
  (translate-dependency-value {:b-cpt :b
                               :c-cpt :c
                               :d-cpt :d}
                              :c :replacement)
  )

(s/fdef merge-dependencies
        :args (s/cat :dependency-template ::dependency-seq
                     :to-replace ::component-name
                     :replacement ::component-name)
        :ret ::dependency-seq)
(defn resolve-nested-dependencies
  "Top-level components can depend on nested, and vice versa

Swap out the alias place-holders, like filling the blanks in a form letter.

e.g. If we have a dependency tree that looks like
{:a [:b :c]
 :b [:c]}
and :b is really a placeholder for :nested, then this should return
{:a [:nested :c]
 :nested [:c]}
Well, except that it really only deals with maps for dependencies.
But that's the general idea.

It seems like I shouldn't need to specify replacement
as an arg. It really should be available somewhere
in the dependency-template.

That may be (but probably isn't) the case. If it is,
that's an accident of the implementation."
  [dependency-template to-replace replacement]
  (->> dependency-template
       (map (fn [[k v]]
              (let [k' (if (= k to-replace)
                         replacement
                         k)
                    v' (translate-dependency-value v to-replace replacement)]
                [k' v'])))
       (into {})))

(s/fdef merge-dependency-trees
        :args (s/cat :acc ::dependency-seq
                     :nested-deps ::dependency-seq)
        :ret ::dependency-seq)
(defn merge-dependency-trees
  "Nested dependencies can have their own dependencies specified at various levels.

Note that this is really meant to be [part of] a reduction.

And that it's really only concerned with the dependency ieces

Say you have a component structure like
  {:a 'top-level-ctor
   :b {::system-configuration {:structure {:nested/database 'database-ctor
                                           :nested/web-server 'web-server-ctor
                                           :nested/whatsit 'whatsit-ctor}
                               :dependencies {:nested/web-server {:database :nested/database}
                                              :nested/database [:nested/whatsit]}
       ::configuration-tree {:nested/database {:url \"database.private.com:19849/connection\"}
                             :nested/web-server {:port 4916}}
       ::primary-component :nested/database}
   :c 'credentials-cpt-ctor}
that has a dependency tree like

  {:a [:b]
   :b {:credentials :c}}

=> the :b component (aka :nested/database) depends on both the :whatsit component (declared
in its nesting layer) and the credentials component (declared at the top of the tree).

This gets more interesting because the outer/top level pieces must override what's declared
at the inner layers because they're closer to the actual usage. This is pretty much exactly
like using ~/.cfg to override a system-level /etc/cfg

In that example, this should be called with parameters
acc: {:a {:b :nested/database}
      :nested/database {:credentials :c}}
nested-deps: {:nested/web-server {:database :nested/database}
                                  :nested/database [:nested/whatsit]}
and should return
  {:a {:b :nested/database}
   :nested/database {:credentials :c
                     :nested/whatsit :nested/whatsit}
   :nested/web-server {:database :nested/database}}
  "
  [acc nested-deps]
  (reduce (fn [acc' [k v]]
            ;; Q: What are the odds that it's this easy?
            (update acc' k (fn [outer]
                             (into v outer))))
          acc
          nested-deps))

(s/fdef merge-nested-struct
        :args (s/cat :root ::initialization-map
                     :de-nesting ::initialization-map)
        :ret ::initialization-map)
(defn merge-nested-struct
  [root de-nesting]
  ;; This approach is too brittle, since I can't namespace the component keys
  ;; (since they're really fields in defrecord instances).
  ;; TODO: Check for duplicates!
  (into root de-nesting))

(s/fdef de-nest-components
        :args (s/cat :acc (s/tuple ::initialization-map ::system-dependencies)
                     :cpt ::initialization-map)
        :ret ::flattened-initialization-map)
(declare pre-process)
(defn de-nest-component-ctors
  "Designed to be the `f` of a reduce.

Takes nested components with their dependencies and recursively promotes them to the top level."
  [{:keys [structure dependencies]
    :as acc}
   [name description]]
  (println (str "\nde-nesting nested Component description " name " --\n" (with-out-str (pprint description))
                "into\n"
                (with-out-str (pprint acc))))
  (let [{:keys [::configuration-tree ::primary-component]} description
        {nested-struct ::structure
         nested-deps ::dependencies
         :as de-nested} (pre-process (::system-configuration description)
                                     configuration-tree)
        duplicates (filter (comp (partial contains? structure) key)
                           nested-struct)]
    (assert (empty? duplicates) (str "Duplicated keys:\n"
                                     duplicates
                                     "\nin\n" structure))
    (if (not (empty? nested-struct))
      (do
        (println (str "component-dsl.system/pre-process  extracted the nested structure\n"
                      (with-out-str (pprint nested-struct))
                      "with dependencies\n"
                      (with-out-str (pprint nested-deps))))
        (assoc acc
               ::dependencies (-> dependencies
                                  (into (dissoc nested-deps primary-component))
                                  (resolve-nested-dependencies name primary-component)
                                  (merge-dependency-trees nested-deps))
               ::structure (merge-nested-struct structure nested-struct)))
      (do
        (println "component-dsl.system/pre-process Hit the bottom")
        (assert (empty? nested-deps))
        acc))))

(s/fdef merge-individual-option
        :args (s/cat :acc ::configuration-tree
                     :options (s/cat :component-name ::component-name
                                     :options ::option-map)))
(defn merge-individual-option
  "This almost definitely needs to be handled with a zipper"
  [acc [component-name options]]
  (update acc component-name
         #(reduce (fn [acc [k v]]
                    (if-not (map? v)
                      (if (contains? acc k)
                        (get acc k)
                        v)
                      (throw (ex-info "Trees will make this interesting"
                                      {:into acc
                                       :for component-name
                                       :merging options}))))
                  (or % {})
                  options)))

(s/fdef de-nest-options
        :args (s/cat :acc ::configuration-tree
                     :options ::nested-definition)
        :ret ::configuration-tree)
(defn de-nest-options
  "Note that values in the acc override the configuration-tree"
  [acc [component-name
        {:keys [::system-configuration ::configuration-tree]
         :as options}]]
  ;; Q: Does this need to recurse?
  (reduce merge-individual-option acc configuration-tree))

(s/fdef pre-process
        :args (s/cat :description ::system-description
                     :options ::configuration-tree)
        :ret ::flattened-description)
(defn pre-process
  "Have to flatten out nested system definitions"
  [{:keys [::structure ::dependencies]
    :as params}
   options]
  (let [tops (->> structure
                  (filter (comp symbol? second))
                  (into {}))
        nested (->> structure
                    (filter (comp (complement symbol?) second))
                    (into {}))]
    (println "pre-processing\n" (with-out-str (pprint nested))
             "\ninto\n" (with-out-str (pprint tops))
             "\nbased on\n" (with-out-str (pprint params)))
    {::description {::structure (reduce de-nest-component-ctors
                                        tops
                                        nested)
                    ::dependencies dependencies}
     ::options (reduce de-nest-options
                       options
                       nested)}))

(s/fdef flatten-options
        :args (s/cat :top-level ::configuration-tree
                     :component-tree ::initialization-map)
        :ret ::configuration-tree)
(defn flatten-options
  "Pull ctor options from the tree of nested components and override them with higher-level ctor options

For performance reasons, it's very tempting to do this in parallel with merging the tree
of structure/dependencies so I don't have to traverse the tree twice.

If your component tree is so big that you have to worry about the performance implications of
traversing it twice...that's the sort of problem that I think I'd like to be solving here.

TODO: Worry about that when the baseline works."
  [top-level component-tree]
  (println "Overriding the options from\n"
           (with-out-str (pprint component-tree))
           "\nwith\n"
           (with-out-str (pprint top-level)))
  (throw (ex-info "Start here" {:todo "Write this"})))

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
        ;;flattened-options (flatten-options options descr)
        pre-init (system-map (::structure pre-processed) #_flattened-options options)]
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
