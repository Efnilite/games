(ns json
  (:require [clojure.string :as str]))

(defn trim-braces [s]
  "Trims braces from JSON object."
  (if (and (= \{ (first s)) (= \} (last s)))
    (subs s 1 (- (count s) 1))
    s))

(defn parts [s]
  "Returns all parts separated by commas on the highest level."
  ((fn step [parts part-so-far rem char-count]
     (let [parts' (conj parts part-so-far)
           c (first rem)
           next? (and (= \, c) (zero? char-count))]
       (if (zero? (count rem))
         parts'
         (step
           (if next? parts' parts)
           (if next? "" (str part-so-far c))
           (rest rem)
           (case c
             \{ (inc char-count)
             \} (dec char-count)
             char-count)))))

   [] "" (trim-braces s) 0))

(defn parse
  "Parses the string as a JSON object."
  [s]
  (->> s
       (parts)
       (map (fn [s'] (mapv str/trim (str/split s' #":" 2))))
       (into {})))