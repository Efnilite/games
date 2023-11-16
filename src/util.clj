(ns util
  (:require [clojure.string :as str]))

(def words (->> (slurp "resources/words.txt")
                (str/split-lines)))

(defn get-random-word [] (rand-nth words))

(defn in?
  "true if coll contains element"
  [coll element]
  (some #(= element %) coll))