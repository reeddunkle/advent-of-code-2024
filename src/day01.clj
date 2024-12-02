(ns day01
  (:require
   [clojure.string :as str]))

(defn abs-diff [a b]
  (Math/abs (- a b)))

(defn unzip [pairs]
  (reduce (fn [[list1 list2] [a b]]
            [(conj list1 a) (conj list2 b)])
          [[] []]
          pairs))

(def sampleInput1 "3   4
4   3
2   5
1   3
3   9
3   3")

(def input (slurp "src/inputs/day01"))

(defn parse [data]
  (let [stringPairs (->> data
                         (str/split-lines)
                         (map #(str/split % #"\s+")))
        numberPairs (->> stringPairs
                         (map (fn [line] (mapv #(Integer/parseInt %) line))))]
    (unzip numberPairs)))

(def parsedInput (parse input))
(def sampleData (parse sampleInput1))

(defn solve1 [lists]
  (->> lists
       (map sort)
       (apply map vector)
       (map #(reduce abs-diff %))
       (reduce +)))

(defn solve2 [lists]
  (let [listA (first lists)
        listB (second lists)
        listBIdByFreq (frequencies listB)]
    (->> listA
         (map (fn [id]
                (let [coefficient (get listBIdByFreq id 0)]
                  (* id coefficient))))
         (reduce +))))

(solve1 sampleData)
(solve1 parsedInput)

(solve2 sampleData)
(solve2 parsedInput)
