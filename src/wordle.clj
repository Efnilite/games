(ns wordle
  (:require [clojure.string :as str]))

(def max-attempt-count 6)

(defn in-word
  "Returns ~ when the char is in word, the char if not."
  [word char]

  (if (util/in? (seq word) char) \~ char))

(defn in-position
  "Returns ✓ when the char is in position, the char if not."
  [word char pos]

  (if (= char (.charAt word pos)) \✓ char))

(defn ask!
  "Asks the next guess."
  [word attempt-count]

  (println)
  (println "Enter a guess.")
  (println "✓ = the letter is on the correct position, ~ = the letter is not on the correct position")
  (println)

  (let [guess (read-line)
        result (map-indexed (fn [idx itm] (in-position word itm idx)) (seq guess))
        result' (map (partial in-word word) (seq result))]
    (do (println) (println (str/join " " (seq guess))) (println (str/join " " result')))
    (cond
      (not= 5 (count guess)) (do (println "Please enter a word with 5 letters.") (ask! word attempt-count))
      (= \✓ (= 1 (->> result' seq distinct count))) (do (println "You won!") (println "The word was" word))
      (= max-attempt-count attempt-count) (do (println "You lost!") (println "The word was" word))
      :else (ask! word (inc attempt-count))
      )))

(ask! (util/get-random-word) 0)