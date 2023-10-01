(ns hangman
  (:require [clojure.string :as str]
            [util :as util]))

(def max-guesses 10)
(def word (util/get-random-word))

(defn display-word
  "Displays the current word progress."
  [previously-guessed]

  (->> (seq word)
       (map (fn [c] (if (util/in? previously-guessed c) c \█)))
       (str/join)))

(defn incorrect-guesses
  "Returns the amount of incorrect guesses"
  [previously-guessed]

  (count (remove (partial util/in? (seq word)) previously-guessed)))

(defn display-guesses
  "Displays the current guess count."
  [previously-guessed]

  (str/join (repeat (- max-guesses (incorrect-guesses previously-guessed)) \█)))

(defn stats
  "Handles stats."
  [previously-guessed]

  (println)
  (println "Your guesses:" (str/join " " (sort (seq previously-guessed))))
  (println "Your remaining guesses:" (display-guesses previously-guessed))
  (println "Your progress:" (display-word previously-guessed)))

(defn ask
  "Asks the next guess."
  [previously-guessed]

  (cond
    (= max-guesses (incorrect-guesses previously-guessed))
    (do (println) (println "You lost!") (println "The word was" word))

    (= (count word) (- (inc (count previously-guessed)) (incorrect-guesses previously-guessed)))
    (do (println) (println "You won!") (println "The word was" word))

    :else
    (let [_ (do (println) (println "Enter a guess."))
          guess (first (char-array (read-line)))
          previously-guessed' (conj previously-guessed guess)]
      (stats previously-guessed')
      (ask previously-guessed'))))

(ask #{})