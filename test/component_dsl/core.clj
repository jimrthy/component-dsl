(ns component-dsl.core
  "Namespace that pretends to create a web server

This and routes really belong under the test folder,
but my unit tests can't find them there.

TODO: Fix that")

(defn ctor
  [_]
  {:routes nil})
