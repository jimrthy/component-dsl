(defproject com.jimrthy/component-dsl "0.1.2-SNAPSHOT"
  :cljsbuild {
              :builds {:dev {:figwheel {:websocket-host "10.0.3.152"}  ; real reason I need port-forwarding for wormtail
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
                 ;; Hmm...fairly major conundrum here.
                 ;; If I upgrade to clojure 1.9, most of the motivation
                 ;; for using prismatic schema seems to go away.
                 ;; Or I could try to support both versions, which seems
                 ;; like a disaster waiting to happen.
                 [org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.293"]
                 [org.clojure/test.check "0.9.0"]
                 ;; Including this here is very debatable.
                 ;; Why would you ever use this without namespace?
                 ;; Honestly, I'm surprised that it isn't part of Component.
                 [org.clojure/tools.namespace "0.2.11"]]
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
  :profiles {:dev {:source-paths ["dev"]}}
  :source-paths ["src"]
  :url "https://github.com/jimrthy/component-dsl")
