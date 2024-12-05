(ns day04 (:require
           [clojure.string :as str]))

(def sampleInput1 (slurp "src/inputs/day04-p1"))
(def sampleInput2 (slurp "src/inputs/day04-p2"))
(def input (slurp "src/inputs/day04"))

;; Notes
;; Horizontal = letters spaced by 1
;; Diagonal diagonal-up-right = letters spaced by length - 1
;; Vertical = letters spaced by line length
;; Diagonal diagonal-down-right = letters spaced by length + 1

(defn is-match [chars]
  (let [joined (str/join "" chars)] (or (= joined "XMAS") (= joined "SAMX"))))

(defn solve1 [data]
  (let [lines (str/split-lines data)
        lineLength (count (first lines))
        steps [1 (- lineLength 1) lineLength (+ lineLength 1)]
        chars (vec (apply str lines))
        total-chars (count chars)
        final-index (- total-chars 4)]
    (reduce
     (fn [total-count idx1]
       (let [inner-count
             (reduce
              (fn [_inner-count step]
                (let [idx2 (+ step idx1)
                      idx3 (+ step idx2)
                      idx4 (+ step idx3)]
                  (cond
                    ;; Skip if indices are out of bounds
                    (>= idx4 total-chars) _inner-count

                    (is-match [(nth chars idx1)
                               (nth chars idx2)
                               (nth chars idx3)
                               (nth chars idx4)])
                    (inc _inner-count)

                    :else _inner-count)))
              0
              steps)]
         (+ total-count inner-count)))
     0
     (range final-index))))

(solve1 (slurp "src/inputs/day04"))

