(defproject crossword "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-cljsbuild "1.1.8"]]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.741"]]
  :repl-options {:init-ns crossword.core}
  :profiles {:dev {:resource-paths ["test/resources"]}}
  :cljsbuild
  {:test-command
   {"node" ["node" :node-runner "target/main.js"]}
   :builds
   [{:id :main
     :source-paths ["src" "test"]
     :compiler
     {:output-to "target/main.js"
      :optimizations :advanced
      :pretty-print false}}]})
