(defproject cypris-form "1.0.3"
  :description "A cljs library to provide a decent interface to forms including state and error handling."
  :url "https://github.com/defkef/cypris-form"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/clojurescript "1.9.229"]]

  :plugins [[com.jakemccrary/lein-test-refresh "0.14.0"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild {
    :builds [{
      :source-paths ["src"]
      :id "main"
      :jar true}]})
