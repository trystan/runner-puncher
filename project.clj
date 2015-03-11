(defproject runner_puncher "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-alpha5"]]
  :plugins [[lein-git-deps "0.0.1-SNAPSHOT"]]
  :git-dependencies [["https://github.com/trystan/super-simple-window.git"]
                     ["https://github.com/trystan/terminal-render.git"]]
  :main runner_puncher.core
  :aot :all
  :uberjar-name "runner_puncher.jar")
