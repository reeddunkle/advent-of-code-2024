(ns day03)

(def sampleInput1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def sampleInput2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(def input (slurp "src/inputs/day03"))

(defn solve1 [data]
  (->> data
       (re-seq #"mul\((\d+),(\d+)\)")
       (map (fn [[_ x y]] (* (Integer/parseInt x) (Integer/parseInt y))))
       (reduce +)))

(defn solve2 [data]
  (->> data
       (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)")
       (reduce (fn [[sum on?] [match x y]]
                 (cond
                   (and on? (some? x) (some? y))
                   [(+ sum (* (Integer/parseInt x)
                              (Integer/parseInt y))) on?]

                   (= match "don't()") [sum false]

                   (= match "do()") [sum true]

                   :else [sum on?]))
               [0 true])
       (first)))

(solve2 input)

