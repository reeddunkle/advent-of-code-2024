(ns day04 (:require
           [clojure.string :as str]))

(defn within-bounds? [y x max-y max-x]
  (and (>= y 0) (< y max-y) (>= x 0) (< x max-x)))

(defn get-coords [lines coords]
  (map (fn [[y x]] (get-in lines [y x])) coords))

(defn calculate-coords [y x steps]
  (map (fn [step]
         [(+ y (first step)) (+ x (second step))])
       steps))

(defn is-match-1 [chars]
  (some #(= "XMAS" %)
        [(str/join "" chars)
         (str/join "" (reverse chars))]))

(defn is-match-2 [chars]
  (some #(= "MAS" %)
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
     (fn [total y]
       (reduce
        (fn [count x]
          (reduce
           (fn [inner-count step]
             (let [calculated-coords (calculate-coords y x step)
                   coords (cons [y x] calculated-coords)
                   [last-y last-x] (last coords)
                   valid? (within-bounds? last-y last-x max-y max-x)]
               (if
                (and valid? (is-match-1 (get-coords lines coords)))
                 (inc inner-count)
                 inner-count)))
           count
           steps))
        total
        (range max-x)))
     0
     (range max-y))))

(defn solve2 [data]
  (let [lines (mapv vec (str/split-lines data))
        max-y (count lines)
        max-x (count (first lines))
        steps [;; Down-right
               [[0 0]
                [1 1]
                [2 2]]
               ;; Down-left
               [[0 2]
                [1 1]
                [2 0]]]]

    (reduce
     (fn [total y]
       (reduce
        (fn [count x]
          (let [coordsA (calculate-coords y x (first steps))
                coordsB (calculate-coords y x (second steps))
                [[last-yA last-xA] [last-yB last-xB]] [(last coordsA) (last coordsB)]
                valid? (and (within-bounds? last-yA last-xA max-y max-x)
                            (within-bounds? last-yB last-xB max-y max-x))]
            (if (and valid?
                     (is-match-2 (get-coords lines coordsA))
                     (is-match-2 (get-coords lines coordsB)))
              (inc count)
              count)))
        total
        (range max-x)))
     0
     (range max-y))))

(solve2 (slurp "src/inputs/day04"))

