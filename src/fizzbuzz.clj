(ns fizzbuzz)

(def stops
  {3 "Fizz"
   5 "Buzz"
   7 "Bizz"
   9 "Fuzz"})

(defn fizzbuzz [n stops str-so-far]
  "Returns the fizzbuzz string."
  (if (empty? stops)
    (if (empty? str-so-far) n str-so-far)
    (let [[k v] (first stops)]
      (fizzbuzz
        n
        (rest stops)
        (if (zero? (mod n k))
          (str str-so-far v)
          str-so-far)))))

(->> (range 1 101)
     (map #(fizzbuzz % stops "")))