(defproject mastering-clojure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/tools.nrepl "0.2.13"]
                 [org.clojure/clojure "1.8.0"]
                 [funcool/cats "2.1.0"]
                 [org.clojure/core.logic "0.8.11"]
                 [org.clojure/core.async "0.3.443"]
                 ;; ^:source-dep [http-kit "2.2.0"]
                 ;; ^:source-dep [cheshire "5.8.0"]
                 ^:source-dep [alembic "0.3.2"]
                 ;; ^:source-dep [org.clojure/tools.analyzer.jvm "0.7.1"]
                 ;; ^:source-dep [org.clojure/tools.namespace  "0.3.0-alpha3"]
                 ;; ^:source-dep [org.clojure/tools.reader "1.1.1"]
                 ;; ^:source-dep [cider/orchard "0.2.0"]
                 ;; ^:source-dep [lein-cljfmt "0.3.0"]
                 ;; ^:source-dep [me.raynes/fs "1.4.6"]
                 ;; ^:source-dep [rewrite-clj "0.6.0"]
                 ;; ^:source-dep [cljs-tooling "0.2.0"]
                 ;; ^:source-dep [version-clj "0.1.2"]
                 ]
  :plugins [[refactor-nrepl "2.3.1"]
            [cider/cider-nrepl "0.14.0"]])
