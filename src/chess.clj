(ns chess
  (:require [clojure.string :as str]))

(def grid
  {[0 0] \♖ [1 0] \♘ [2 0] \♗ [3 0] \♕ [4 0] \♔ [5 0] \♗ [6 0] \♘ [7 0] \♖
   [0 1] \♙ [1 1] \♙ [2 1] \♙ [3 1] \♙ [4 1] \♙ [5 1] \♙ [6 1] \♙ [7 1] \♙

   [0 6] \♟ [1 6] \♟ [2 6] \♟ [3 6] \♟ [4 6] \♟ [5 6] \♟ [6 6] \♟ [7 6] \♟
   [0 7] \♜ [1 7] \♞ [2 7] \♝ [3 7] \♛ [4 7] \♚ [5 7] \♝ [6 7] \♞ [7 7] \♜})

(defn print-grid! [grid]
  "Prints the grid."
  (->> (range 8)
       (map (fn [y] (->> (range 8)
                         (map (fn [x] (get grid [x y] \▭)))
                         (str/join " "))))
       (reverse)
       (str/join "\n")
       (format)
       (println)))

(defn letter-number-to-pos [[l n]]
  "Transforms letter-number to pos, e.g. 'a1' to [0 0]."
  [((zipmap (seq "abcdefgh") (range 8)) l)
   (dec (Integer/parseInt (Character/toString (char n))))])

(defn black? [c]
  "Returns true when a char is black, nil if not."
  (some (partial = c) (seq "♙♖♘♗♕♔")))

(defn white? [c]
  "Returns true when a char is white, nil if not."
  (some (partial = c) (seq "♟♜♞♝♛♚")))

(defn can-move-to? [[x y] [x-to y-to] grid]
  "Returns true when the chess piece at [x y] can move to [x-to y-to] in grid, false if not."
  (let [at (grid [x y])
        to (grid [x-to y-to])

        can-move-map
        {\♙
         (or
           (and (= x x-to) (= 1 (- y-to y)) (nil? to))
           (and (= x x-to) (= 2 (- y-to y)) (= y 1) (nil? to))
           (and (= 1 (abs (- x-to x))) (= 1 (- y-to y)) (white? to)))
         \♟
         (or
           (and (= x x-to) (= -1 (- y-to y)) (nil? to))
           (and (= x x-to) (= 2 (- y-to y)) (= y 6) (nil? to))
           (and (= 1 (abs (- x-to x))) (= -1 (- y-to y)) (black? to)))

         \♔
         (and (or (= 1 (abs (- x-to x))) (= 1 (abs (- y-to y))))
              (or (nil? to) (white? to)))
         \♚
         (and (or (= 1 (abs (- x-to x))) (= 1 (abs (- y-to y))))
              (or (nil? to) (black? to)))

         \♕
         (and (or (= (- x-to x) (- y-to y))
                  (= x x-to)
                  (= y y-to))
              (or (nil? to) (white? to)))
         \♛
         (and (or (= (- x-to x) (- y-to y))
                  (= x x-to)
                  (= y y-to))
              (or (nil? to) (black? to)))

         \♖
         (and (or (= x x-to) (= y y-to))
              (or (nil? to) (white? to)))
         \♜
         (and (or (= x x-to) (= y y-to))
              (or (nil? to) (black? to)))

         \♗
         (and (= (- x-to x) (- y-to y))
              (or (nil? to) (white? to)))
         \♝
         (and (= (- x-to x) (- y-to y))
              (or (nil? to) (black? to)))

         \♘
         (and (or (and (= 3 (abs (- x-to x))) (= 1 (abs (- y-to y))))
                  (and (= 1 (abs (- x-to x))) (= 3 (abs (- y-to y)))))
              (or (nil? to) (white? to)))
         \♞
         (and (or (and (= 3 (abs (- x-to x))) (= 1 (abs (- y-to y))))
                  (and (= 1 (abs (- x-to x))) (= 3 (abs (- y-to y)))))
              (or (nil? to) (black? to)))
         }]
    (can-move-map at)))

(defn step! [grid]
  (print-grid! grid)
  (let [nx (str/split (read-line) #":")
        pos (letter-number-to-pos (first nx))
        to (letter-number-to-pos (last nx))
        value (grid pos)]
    (if (not (can-move-to? pos to grid))
      (let []
        (println "Invalid move")
        (step! grid))
      (step! (-> grid
                 (dissoc pos)
                 (assoc to value))))))

(step! grid)