(defproject blackwaterpark/data.csv "1.1.2"
  :description "A Clojure library for reading and writing comma separated value (csv) files (Eva fork)"
  :url "https://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "https://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/test.check "1.1.0"]]  
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})