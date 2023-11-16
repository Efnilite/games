(ns g2048
  (:require [clojure.string :as str]))

(def width 4)

(defn adjacent-numbers [[x y] grid]
  "Returns the numbers in adjacent positions to [x y] in grid."
  (->> [[x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]]
       (filter grid)
       (map grid)))

(defn game-over? [grid]
  "Returns true when the player can't make any more moves"
  (if (not= (count grid) (* width width))
    false
    (->> grid
         (some (fn [[k v]]
                 (->> grid
                      (adjacent-numbers k)
                      (some (partial = v)))))
         (not))))

(defn print-grid! [grid]
  "Prints the grid"
  (let [available (range width)
        str-length (->> grid (vals) (apply max) (str) (count))
        pad (fn [s] (format (str "%" (- str-length) "s") s))]
    (->> available
         (map (fn [y] (->> available
                           (map (fn [x] (get grid [x y] "")))
                           (map pad)
                           (str/join "  "))))
         (reverse)
         (str/join "\n")
         (format)
         (println))))

(defn next-pos [pos value dir grid]
  "Returns the next closest available spot for this position."
  (let [pos' (mapv + pos dir)
        not-nil= (fn [x y] (and (not (nil? x)) (not (nil? y)) (= x y)))]
    (cond (or (> (first pos') (dec width))
              (> (second pos') (dec width))
              (< (first pos') 0)
              (< (second pos') 0)) pos
          (not-nil= (grid pos') value) pos'
          (grid pos') pos
          :else (next-pos pos' value dir grid))))

(defn move [dir new-grid old-grid]
  "Moves all positions to their new positions. Old grid should be sorted by execution order."
  (if (empty? old-grid)
    new-grid
    (let [head (first old-grid)
          pos (first head)
          value (second head)
          pos' (next-pos pos value dir new-grid)]
      (move dir
            (if (and (not= pos pos') (= (new-grid pos') value))
              (assoc new-grid pos' (* 2 value))
              (assoc new-grid pos' value))
            (rest old-grid)))))

(defn random-available-pos [grid]
  "Returns a random available position."
  (let [available (range width)]
    (->> (for [x available y available] [x y])
         (remove (set (keys grid)))
         (rand-nth))))

(defn lose! [grid score moves]
  (println)
  (println "You lost!")
  (println "Score:" score)
  (println "Moves:" moves)
  (println)
  (println "Final grid:")
  (print-grid! grid)
  nil)

(defn step! [grid score moves]
  (if (game-over? grid)
    (lose! grid score moves)
    (let [input (str/lower-case (read-line))
          dir (case input
                "w" [[0 1] (partial sort (fn [[[_ y1] _] [[_ y2] _]] (compare y2 y1)))]
                "s" [[0 -1] (partial sort (fn [[[_ y1] _] [[_ y2] _]] (compare y1 y2)))]
                "a" [[-1 0] (partial sort (fn [[[x1 _] _] [[x2 _] _]] (compare x1 x2)))]
                "d" [[1 0] (partial sort (fn [[[x1 _] _] [[x2 _ ] _]] (compare x2 x1)))]
                (step! grid score moves))
          grid' (->> grid ((second dir)) (move (first dir) {}))
          grid' (assoc grid' (random-available-pos grid') 2)]
      (print-grid! grid')
      (step! grid'
            score
            (inc moves)))))

(step! {[0 0] 2 [1 1] 2} 0 0)