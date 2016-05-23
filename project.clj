(defproject com.jimrthy/component-dsl "0.1.1-SNAPSHOT"
  :cljsbuild {
              :builds {:dev {:figwheel {:websocket-host "10.0.3.152"}
                             :source-paths ["src/cljs" "dev_src/cljs"]
                             ;; Different output targets should go to different paths
                             ;; Should probably have a different index.?.html based on build
                             ;; profile.
                             ;; Then the route middleware that returns the index could return
                             ;; the one based on build profile (assuming that info's available
                             ;; at run-time)
                             :compiler {:output-to "resources/public/js/compiled/frereth.js"
                                        :output-dir "resources/public/js/compiled"
                                        :optimizations :none
                                        :main frereth.core   ; Q: Huh?
                                        ;;:main frereth.core
                                        :asset-path "js/compiled"
                                        ;;:source-map "resources/public/js/compiled/frereth.js.map"
                                        :source-map true
                                        :source-map-timestamp true
                                        :verbose true
                                        ;;:cache-analysis true
                                        }}

                       ;; TODO: Compare the output size of this vs. standard
                       ;; minification
                       :min {:source-paths ["src/cljs" "dev_src/cljs"]
                             :compiler {:output-to "resources/public/js/compiled/frereth.js"
                                        :main frereth.core
                                        ;; TODO: Advanced compilation has gone away
                                        ;; Actually, the entire google.clojure compiler has gone away
                                        ;; Q: Why am I getting errors from that?
                                        :optimizations :advanced
                                        :pretty-print false}}}}
  :dependencies [[com.stuartsierra/component "0.3.1"]
                 ;; TODO: Eliminate this dependency
                 [org.clojure/clojure "1.8.0"]
                 ;; Q: Can this one go away?
                 ;; It pretty definitely needs to be updated.
                 [org.clojure/clojurescript "1.8.51"]
                 ;; Including this here is very debatable, but
                 ;; it's a lot safer than schema.
                 ;; Why would you ever use this without namespace?
                 ;; Honestly, I'm surprised that it isn't part of Component.
                 [org.clojure/tools.namespace "0.2.11"]
                 [prismatic/schema "1.1.1"]]
  :description "Describe your Components in EDN"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild "1.1.2" :exclusions [org.clojure/clojure]]
            ;; TODO: This dependency should go away.
            ;; It's only here because of a screwy interaction
            ;; with cider dependencies
            ;; At the very least, it only belongs under
            ;; the dev profile
            #_[org.clojure/java.classpath "0.2.3"]]
  :source-paths ["dev" "src"]
  :url "https://github.com/jimrthy/component-dsl")
