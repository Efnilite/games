(ns json
  (:require [clojure.string :as str]))

; todo commas in strings causing problems seperating in parts method

(defn trim-braces [s]
  "Trims braces from JSON object."
  (if (and (= \{ (first s)) (= \} (last s)))
    (subs s 1 (- (count s) 1))
    s))

(defn parts [s]
  "Returns all parts separated by commas on the highest level."
  ((fn step [parts part-so-far rem char-count]
     (let [parts' (conj parts (str/trim part-so-far))
           c (first rem)
           next? (and (= \, c) (zero? char-count))]
       (if (zero? (count rem))
         parts'
         (step
           (if next? parts' parts)
           (if next? "" (str part-so-far c))
           (rest rem)
           (cond
             (or (= c \{) (= c \[)) (inc char-count)
             (or (= c \}) (= c \])) (dec char-count)
             :else char-count)))))

   [] "" s 0))

(defn parse-value [s]
  "Parses a value to a primitive or string."
  (cond (re-matches #"[0-9]*" s) (Integer/parseInt s)
        (re-matches #"[0-9]+\.[0-9]+" s) (parse-double s)
        (re-matches #"(true|false)" s) (parse-boolean s)
        (= "null" s) nil
        :else s))

(defn parse [s]
  "Parses the string as a JSON object."
  (let [trimmed (trim-braces s)]
    (cond
      (re-matches #"\[(.*)\]" trimmed)
      (as-> (subs s 1 (- (count s) 1)) s'
            (str/split s' #",")
            (mapv (comp str/trim parse) s'))

      (= trimmed s)
      (parse-value s)

      :else
      (->> (parts trimmed)
           (map (fn [s'] (mapv str/trim (str/split s' #":" 2))))
           (map (fn [[k v]] [k (parse v)]))
           (into {})))))

(defn stringify [map]
  "Stringifies a map to a JSON object."

  )