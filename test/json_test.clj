(ns json-test
  (:require [clojure.test :refer :all :as test]))

(deftest test-parse-json
  (testing "Parsing an empty JSON object"
    (is (= (json/parse "{}") {})))

  (testing "Parsing a JSON object with one key-value pair"
    (is (= (json/parse "{\"key\": \"value\"}") {"key" "value"})))

  (testing "Parsing a JSON object with multiple key-value pairs"
    (is (= (json/parse "{\"key1\": \"value1\", \"key2\": 2}")
           {"key1" "value1" "key2" 2})))

  (testing "Parsing an empty JSON array"
    (is (= (json/parse "[]") [])))

  (testing "Parsing a JSON array with one element"
    (is (= (json/parse "[\"element\"]") ["element"])))

  (testing "Parsing a JSON array with multiple elements"
    (is (= (json/parse "[\"element1\", \"element2\"]") ["element1", "element2"])))

  (testing "Parsing a JSON integer"
    (is (= (json/parse "1") 1)))

  (testing "Parsing a JSON object with integer value"
    (is (= (json/parse "{\"key\": 1}") {"key" 1}))))

(deftest test-parse-errors
  (testing "Parsing a non-JSON string"
    (is (test/thrown? Exception (json/parse "not a json string")))))

(defn -main []
  (test/run-tests 'json-test))