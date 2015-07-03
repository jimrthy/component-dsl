(defproject com.jimrthy/component-dsl "0.1.1-SNAPSHOT"
  :description "Describe your Components in EDN"
  :url "https://github.com/jimrthy/component-dsl"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[com.stuartsierra/component "0.2.3"]
                 [org.clojure/clojure "1.7.0"]
                 [prismatic/schema "0.4.3"]]
  :plugins [
            ;; TODO: This dependency should go away.
            ;; It's only here because of a screwy interaction
            ;; with cider dependencies
            ;; At the very least, it only belongs under
            ;; the dev profile
            [org.clojure/java.classpath "0.2.2"]]
  :source-paths ["dev" "src"])
