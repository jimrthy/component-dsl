(ns component-dsl.done-manager
  "I keep finding myself needing something my -main can deref so it blocks
while everything else interesting happens.

  I've found this useful for that, though it obviously isn't limited to this use case"
  (:require [com.stuartsierra.component :as component]))

(defn ctor [options]
  {:done (promise)})
