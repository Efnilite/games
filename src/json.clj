(ns json
  (:require [clojure.string :as str]))

(defn- trim-braces [s]
  "Trims braces from JSON object."
  (if (and (= \{ (first s)) (= \} (last s)))
    (subs s 1 (- (count s) 1))
    s))

(defn- parts [s]
  "Returns all parts separated by commas on the highest level."
  (loop [parts [] part-so-far "" rem s char-count 0 in-string? false]
     (let [parts' (conj parts part-so-far)
           c (first rem)
           next? (and (= \, c) (zero? char-count) (not in-string?))]
       (if (zero? (count rem))
         parts'
         (recur
           (if next? parts' parts)
           (if next? "" (str part-so-far c))
           (rest rem)
           (cond
             (or (= c \{) (= c \[)) (inc char-count)
             (or (= c \}) (= c \])) (dec char-count)
             :else char-count)
           (if (not in-string?)
             (or (= c \") (= c \'))
             (and (not (or (= c \") (= c \'))) (not (= \\ (last part-so-far))))))))))

(defn- parse-key [s]
  "Parses a key to a string or keyword."
  (if (re-matches #"(\"|')(.*)(\"|')" s)
    (-> (re-find #"(\"|')(.*)(\"|')" s)
        (nth 2)
        (keyword))

    (keyword s)))

(defn parse-value [s]
  "Parses a value to a primitive or string."
  (cond (re-matches #"(\"|')(.*)(\"|')" s) (nth (re-find #"(\"|')(.*)(\"|')" s) 2)
        (re-matches #"[0-9]*" s) (Integer/parseInt s)
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
            (str/split s' #", ?")
            (mapv parse s'))

      (= trimmed s)
      (parse-value s)

      :else
      (->> (parts trimmed)
           (map (fn [s'] (str/split s' #": ?" 2)))
           (map (fn [[k v]] [(parse-key k) (parse v)]))
           (into {})))))

(defn stringify [x]
  "Stringifies x to a JSON object."
  (cond (map? x)
        (str "{"
             (->> x
                  (map (fn [[k v]] (str "\"" (name k) "\": " (stringify v))))
                  (str/join #", "))
             "}")

        (or (vector? x) (list? x))
        (str "["
             (->> x
                  (map stringify)
                  (str/join #", "))
             "]")

        :else x))