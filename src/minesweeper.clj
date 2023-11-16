(ns minesweeper
  (:require [clojure.string :as str]))

(def dimensions [40 10])

(defn adjacent-8 [[x y]]
  "Returns all valid coordinates adjacent to the given coordinates."
  [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
   [(dec x) y] [(inc x) y]
   [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]])

(defn adjacent-4 [[x y]]
  "Returns all valid coordinates adjacent to the given coordinates."
  [[x (inc y)]
   [(dec x) y] [(inc x) y]
   [x (dec y)]])

(defn mines-adjacent [pos grid]
  "Gets the amount of mines adjacent to the pos in grid."
  (->> pos
       (adjacent-8)
       (filter (fn [pos'] (= \█ (get grid pos'))))
       (count)))

(defn generate-grid []
  "Generates a grid with mines at random locations and then the mine proximity numbers. Returns new grid."
  (let [grid-mines-only
        (->> (fn [] [(rand-int (first dimensions)) (rand-int (second dimensions))])
             (repeatedly 40)
             (reduce (fn [old-grid pos] (assoc old-grid pos \█)) {}))]

    (->> (for [x (range (first dimensions))
               y (range (second dimensions))]
           [[x y] (get grid-mines-only [x y])])

         (map (fn [[pos c]]
                [pos
                 (let [adjacents (mines-adjacent pos grid-mines-only)]
                   (cond (some? c) \█
                         (zero? adjacents) \░
                         :else adjacents))]))

         (into {}))))

(defn all-tiles [start grid]
  "Returns all tiles which should be revealed if the player decides to select tile 'start'."
  (let [all (fn all [pos so-far grid]
              (let [value (get grid pos)
                    so-far' (conj so-far pos)]
                (if (not= \░ value)
                  so-far'
                  (->> pos
                       (adjacent-4)
                       (remove (set so-far'))
                       (mapcat (fn [pos'] (all pos' so-far' grid)))))))]
    (->> (all start [] grid)
         (distinct))))

(defn print-grid! [grid unlocked]
  "Prints the grid."
  (let [xs (range (first dimensions))
        ys (range (second dimensions))]
    (->> ys
         (map (fn [y] (->> xs
                           (map (fn [x] (if (util/in? unlocked [x y]) (get grid [x y]) \█)))
                           (str/join ""))))
         (reverse)
         (str/join "\n")
         (format)
         (println))))

(defn lose! [grid unlocked score]
  "Handles losing."
  (println)
  (println "You lost!")
  (println)
  (println "Your score:" score)
  (println "Final grid:")
  (print-grid! grid unlocked)
  (println))

(defn step [grid unlocked score]
  (print-grid! grid unlocked)
  (let [pos (->> (str/split (read-line) #" ")
                 (map #(Integer/parseInt %)))
        value (get grid pos)]
    (if (= value \█)
      (lose! grid unlocked score)
      (let [unlocked' (->> (all-tiles pos grid)
                           (reduce conj unlocked))]
        (step grid unlocked' (inc score))))))

(step (generate-grid) [] 0)