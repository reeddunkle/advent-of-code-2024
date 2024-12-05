(ns day04 (:require
           [clojure.string :as str]))

(defn within-bounds? [y x max-y max-x]
  (and (>= y 0) (< y max-y) (>= x 0) (< x max-x)))

(defn get-coords [lines coords]
  (map (fn [[y x]] (get-in lines [y x])) coords))

(defn is-match [chars]
  (some #(= "XMAS" %)
        [(str/join "" chars)
         (str/join "" (reverse chars))]))

(defn solve1 [data]
  (let [lines (mapv vec (str/split-lines data))
        max-y (count lines)
        max-x (count (first lines))
        steps [[[0 1] [0 2] [0 3]] ;; Horizontal
               [[1 -1] [2 -2] [3 -3]] ;; Diagonal left
               [[1 0] [2 0] [3 0]] ;; Vertical
               [[1 1] [2 2] [3 3]]] ;; Diagonal right
        ]
    (reduce
     (fn [total y1]
       (reduce
        (fn [count x1]
          (reduce
           (fn [inner-count step]
             (let [coords (map #(vector (+ y1 (first %)) (+ x1 (second %))) step)
                   [last-y last-x] (last coords)
                   valid? (within-bounds? last-y last-x max-y max-x)]
               (if
                (and valid? (is-match (get-coords lines (cons [y1 x1] coords))))
                 (inc inner-count)
                 inner-count)))
           count
           steps))
        total
        (range max-x)))
     0
     (range max-y))))

(solve1 (slurp "src/inputs/day04"))

