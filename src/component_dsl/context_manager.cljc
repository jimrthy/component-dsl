(ns component-dsl.context-manager
  "Because you have to define your system somewhere

  It could be in a global var (which violates the workflow-reloaded
  contract but is convenient for REPL-driven development)

  Or you can pass the system around everywhere to everything that
  actually needs it, but that gets annoying.

  Or it could be in a monadic context, which is the basic idea I'm
  trying to set up here."
  (:require [clojure.pprint :refer (pprint)]
            [clojure.spec :as s]
            [com.stuartsierra.component])
  (:import [com.stuartsierra.component SystemMap]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

;; TODO: ^:always-validate
(s/fdef create-context
        :args (s/cat :system :component-dsl.system/system-map)
        :ret (s/fspec :args any?
                      ;; This seems like an excellent time for the :fn part of the definition.
                      ;; I'm not sure what I could add that's simpler/easier than the
                      ;; body, though.
                      :ret any?))
(defn create-context
  "Sets up a context for invoking functions with a system

@param system
@return context function that can be called with another function and most of its args

That function will have the system injected as its first parameter"
  [system]
  (fn [f & args]
    (apply f system args)))

(defn context!
  "Actual run-time pieces that need access to the running system should
  call this with the function they want to call and its associated
  arguments.

  This will call the function you supplied with the system inserted as
  the first argument.

  Well, it well after you actually alter this to make it work.

  Create a context by calling create-context, then call set-global-context!
  on that so you can actually use it through this, without needing to pass
  it around explicitly.

  Note that this destroys functional purity."
  [f & args]
  (throw (ex-info "System not started"
                  {:problem "Replace this var with one that works"})))

;; TODO: ^:always-validate
(s/fdef set-global-context! :args (s/cat :system :component-dsl.system/system-map))
(defn set-global-context!
  "This is probably the most controversial part."
  [system]
  (alter-var-root (var context!)
                  (constantly (create-context system))))

;;; Explaining macros is one of the primary goals behind clojure.spec.
(s/fdef with-component!
        :args (s/cat :component (s/fspec :args (s/cat :finder any?)
                                         ;; This actually returns a Component.
                                         ;; But we really don't know anything about those,
                                         ;; except that they're objects that implement Lifecycle
                                         ;; and something like IAssociative.
                                         ;; Might be able to narrow this down a bit, but it
                                         ;; doesn't seem all that valuable.
                                         :ret any?)
                     :parameters (s/coll-of simple-symbol?)
                     ;; TODO: Is it worth the effort to try to spec out body?
                     ;; Surely this is already done in a ton of existing core macros
                     :body (s/* any?))
        :ret any?)
(defmacro with-component!
  "Work with a specific component from the global system context.
  @param component: function that finds the Component of the System that interests you.
  Usually just a keyword

  @param parameters: vector of the parameter names that body uses.
  Your component is first.

  @param body: sequence of forms that will be called with the
  specificed component attached to the symbol that's the first member
  of parameters"
  [component
   parameters
   & body]
  (let [all-params parameters
        ;; The first parameter has the system, which
        ;; we need to replace with the specifically
        ;; requested component
        component-param (first all-params)
        ;; And these do the actual function call
        others (rest all-params)]
    `(let [g# (fn ~all-params
                ;; Q: How can this possibly be correct?
                ;; Q: Am I actually using this anywhere? Like, say, a unit test?
                ;; This really seems like it should just be syntactic sugar over
                ;; context!
                ;; TODO: Revisit this and figure out what exactly is
                ;; going on.
                (let [~component-param (~component ~component-param)]
                  ~@body))]
       (context! g#  ~@others))))
