(ns component-dsl.context-manager
  "Because you have to define your system somewhere

  It could be in a global var (which violates the workflow-reloaded
  contract but is convenient for REPL-driven development)

  Or you can pass the system around everywhere to everything that
  actually needs it, but that gets annoying.

  Or it could be in a monadic context, which is the basic idea I'm
  trying to set up here."
  (:require [clojure.pprint :refer (pprint)]
            [com.stuartsierra.component]
            [schema.core :as s #?@(:cljs (:include-macros true))])
  (:import [com.stuartsierra.component SystemMap]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(defn context
  "Actual run-time pieces that need access to the running system should
  call this with the function they want to call and its associated
  arguments.

  This will call the function you supplied with the system inserted as
  the first argument.

  Well, it well after you actually alter this to make it work.
"
  [f & args]
  (throw (ex-info "System not started"
                  {:problem "Replace this var with one that works"})))

(s/defn setup
  "This is the part that sets up context. Call it with your started system."
  [system :- SystemMap]
  (alter-var-root (var context)
                  (fn [_]  ;; does the altering
                    (fn [f & args]  ;; what it returns now
                      (apply f system args)))))

(defmacro with-component
  "Work with a specific component from the system stored in context.
  Parameters:
  component: function that finds the Component of the System that interests you.
  Usually just a keyword

  parameters: vector of the parameter names that body uses.
  Your component is first.

  body: single form that will be called w"
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
                (let [~component-param(~component ~component-param)]
                  ~@body))]
       (context g#  ~@others))))
