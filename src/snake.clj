(ns snake
  (:require [clojure.string :as str]))

(def grid {:snake [[3 8] [2 8] [1 8]]
           :candy [6 8]})

(defn print-grid! [grid]
  "Prints the grid."
  (let [snake (grid :snake)
        candy (grid :candy)]
    (->> (range 16)
         (map (fn [y] (->> (range 16)
                           (map (fn [x]
                                  (cond (util/in? snake [x y]) \█
                                        (= [x y] candy) \C
                                        :else " ")))
                           (str/join " "))))
         (reverse)
         (str/join "\n")
         (format)
         (println))))

(defn move-snake [snake next-head idx]
  "Moves the snake with the new head being next-head."
  (if (= idx (count snake))
    snake
    (move-snake
      (assoc snake idx next-head)
      (nth snake idx nil)
      (inc idx))))

(defn generate-candy [grid]
  "Generates a candy at a random available spot. Returns new grid."
  (assoc grid
    :candy
    (let [snake (set (grid :snake))]
      (->> (for [x (range 16) y (range 16)] [x y])
           (remove snake)
           (rand-nth)))))

(defn lost? [snake head']
  "Returns true when the game has been lost."
  (let [x (first head')
        y (last head')]
    (or (util/in? snake head')
        (< x 0) (> x 15)
        (< y 0) (> y 15))))

(defn step! [grid]
  (print-grid! grid)
  (let [c (str/lower-case (read-line))
        dir (case c
              "w" [0 1]
              "a" [-1 0]
              "d" [1 0]
              [0 -1])
        candy (grid :candy)
        snake (grid :snake)
        head (nth snake 0)
        head' (mapv + head dir)
        last (last snake)
        snake' (move-snake snake head' 0)
        grid' (assoc grid :snake (if (= head' candy)
                                   (conj snake' last)
                                   snake'))]
    (if (lost? snake head')
      (println "You failed!")
      (step!
        (if (= head' candy)
          (generate-candy grid')
          grid')))))

(step! grid)