(defproject org-parser "0.1.0-SNAPSHOT"
  :description "parser for emacs org files"
  :url "www.github.com/anuragpeshne/org-parser"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.748"]]
  :plugins [[lein-cljsbuild "1.1.8"]]
  :repl-options {:init-ns org-parser.core}
  :cljsbuild {:builds [{:source-paths ["src"]
                        :compiler {:output-to "target/javascripts/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]})
