(ns clojure.data.csv-test
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as props]
   [clojure.data.csv :refer [read-csv write-csv]])
  (:import
   [java.io StringWriter]))

(def ^{:private true} simple
  "Year,Make,Model
1997,Ford,E350
2000,Mercury,Cougar
")

(def ^{:private true} simple-alt-sep
  "Year;Make;Model
1997;Ford;E350
2000;Mercury;Cougar
")

(def ^{:private true} complicated
  "1997,Ford,E350,\"ac, abs, moon\",3000.00
1999,Chevy,\"Venture \"\"Extended Edition\"\"\",\"\",4900.00
1999,Chevy,\"Venture \"\"Extended Edition, Very Large\"\"\",\"\",5000.00
1996,Jeep,Grand Cherokee,\"MUST SELL!
air, moon roof, loaded\",4799.00")

(deftest reading
  (let [csv (read-csv simple)]
    (is (= (count csv) 3))
    (is (= (count (first csv)) 3))
    (is (= (first csv) ["Year" "Make" "Model"]))
    (is (= (last csv) ["2000" "Mercury" "Cougar"])))
  (let [csv (read-csv simple-alt-sep :separator \;)]
    (is (= (count csv) 3))
    (is (= (count (first csv)) 3))
    (is (= (first csv) ["Year" "Make" "Model"]))
    (is (= (last csv) ["2000" "Mercury" "Cougar"])))
  (let [csv (read-csv complicated)]
    (is (= (count csv) 4))
    (is (= (count (first csv)) 5))
    (is (= (first csv)
           ["1997" "Ford" "E350" "ac, abs, moon" "3000.00"]))
    (is (= (last csv)
           ["1996" "Jeep" "Grand Cherokee", "MUST SELL!\nair, moon roof, loaded" "4799.00"]))))
        

(deftest reading-and-writing
  (let [string-writer (StringWriter.)]
    (->> simple read-csv (write-csv string-writer))
    (is (= simple
	   (str string-writer)))))

(deftest throw-if-quoted-on-eof
  (let [s "ab,\"de,gh\nij,kl,mn"]
    (try
      (doall (read-csv s))
      (is false "No exception thrown")
      (catch Exception e
        (is (or (instance? java.io.EOFException e)
                (and (instance? RuntimeException e)
                     (instance? java.io.EOFException (.getCause e)))))))))

(deftest parse-line-endings
  (let [csv (read-csv "Year,Make,Model\n1997,Ford,E350")]
    (is (= 2 (count csv)))
    (is (= ["Year" "Make" "Model"] (first csv)))
    (is (= ["1997" "Ford" "E350"] (second csv))))
  (let [csv (read-csv "Year,Make,Model\r\n1997,Ford,E350")]
    (is (= 2 (count csv)))
    (is (= ["Year" "Make" "Model"] (first csv)))
    (is (= ["1997" "Ford" "E350"] (second csv))))
  (let [csv (read-csv "Year,Make,Model\r1997,Ford,E350")]
    (is (= 2 (count csv)))
    (is (= ["Year" "Make" "Model"] (first csv)))
    (is (= ["1997" "Ford" "E350"] (second csv))))
  (let [csv (read-csv "Year,Make,\"Model\"\r1997,Ford,E350")]
    (is (= 2 (count csv)))
    (is (= ["Year" "Make" "Model"] (first csv)))
    (is (= ["1997" "Ford" "E350"] (second csv)))))

(deftest writing-quotes-with-different-escape-char
  (let [data [["This string \"contains\" quotes"]]
        writer (StringWriter.)]
    (write-csv writer data :escape \\)
    (is (= "\"This string \\\"contains\\\" quotes\"\n" (str writer)))))

(deftest writing-escape-char-with-different-escape-char
  (let [data [["This string contains the escape char\\"]]
        writer (StringWriter.)]
    (write-csv writer data :escape \\)
    (is (= "This string contains the escape char\\\n" (str writer)))))

(deftest reading-quotes-with-different-escape-char
  (let [input "Key,Value\nExample,\"This \\\"contains\\\" quotes\""
        csv (read-csv input :escape \\)]
    (is (= 2 (count csv)))
    (is (= ["Key" "Value"] (first csv)))
    (is (= ["Example" "This \"contains\" quotes"] (second csv)))))

(deftest reading-escape-char-with-different-escape-char
  (let [input "Key,Value\nExample,\"This contains the escape char \\\\\""
        csv (read-csv input :escape \\)]
    (is (= 2 (count csv)))
    (is (= ["Key" "Value"] (first csv)))
    (is (= ["Example", "This contains the escape char \\"] (second csv)))))

(defspec roundtripping 250
  (props/for-all [data (gen/vector (gen/vector gen/string 2 50) 1 100) ; 1-100 rows of 2-50 columns
                  escape-char gen/char
                  newline (gen/elements [:lf :cr+lf])]
                 (let [write (fn [data]
                               (let [writer (StringWriter.)]
                                 (write-csv writer data
                                            :escape escape-char
                                            :newline newline)
                                 (str writer)))
                       csv (write data)                       
                       expected-result (when (not= [[]] data)                                         
                                         data)
                       actual-result (read-csv csv 
                                          :escape escape-char
                                          :newline newline)]
                   (is (= expected-result actual-result)))))