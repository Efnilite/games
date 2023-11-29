(ns json-test
  (:require [clojure.test :refer :all :as test]
            [json :refer :all]))

(deftest test-parse-json
  (testing "Parsing an empty JSON object"
    (is (= (json/parse "{}") {})))

  (testing "Parsing a JSON object with one key-value pair"
    (is (= (json/parse "{\"key\": \"value\"}") {:key "value"})))

  (testing "Parsing a JSON object with multiple key-value pairs"
    (is (= (json/parse "{\"key1\": \"value1\", \"key2\": 2}")
           {:key1 "value1" :key2 2})))

  (testing "Parsing an empty JSON array"
    (is (= (json/parse "[]") [])))

  (testing "Parsing a JSON array with one element"
    (is (= (json/parse "[\"element\"]") ["element"])))

  (testing "Parsing a JSON array with multiple elements"
    (is (= (json/parse "[\"element1\", \"element2\"]") ["element1", "element2"])))

  (testing "Parsing a JSON object with integer value"
    (is (= (json/parse "{\"key\": 1}") {:key 1})))

  (testing "Parsing a complex JSON object"
    (is (= (json/parse "{\"k\": {\"a\": \"b\"}, \"v\": [1, 2, \"3\"]}") {:k {:a "b"} :v [1 2 "3"]}))))

(deftest test-parse-value
  (testing "Parsing a string"
    (is (= (json/parse-value "hello") "hello")))

  (testing "Parsing an integer"
    (is (= (json/parse-value "123") 123)))

  (testing "Parsing a double"
    (is (= (json/parse-value "123.45") 123.45)))

  (testing "Parsing a boolean"
    (is (= (json/parse-value "true") true))
    (is (= (json/parse-value "false") false)))

  (testing "Parsing null"
    (is (nil? (json/parse-value "null")))))

  ;(testing "Parsing an invalid string"
  ;  (is (test/thrown? IllegalArgumentException (json/parse-single "invalid")))))

;(deftest test-parse-errors
;  (testing "Parsing a non-JSON string"
;    (is (test/thrown? Exception (json/parse "not a json string")))))

(defn -main []
  (test/run-tests 'json-test))