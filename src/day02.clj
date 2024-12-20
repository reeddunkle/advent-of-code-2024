(ns day02
  (:require
   [clojure.string :as str]))

;; Util

(defn abs-diff [a b]
  (Math/abs (- a b)))

(defn pairwise-compare [predicate list]
  (or (empty? list)
      (empty? (rest list))
      (and (predicate (first list) (second list))
           (pairwise-compare predicate (rest list)))))

(defn ascending? [list]
  (pairwise-compare < list))

(defn descending? [list]
  (pairwise-compare > list))

(defn omit-by-index [omitted-index collection]
  (keep-indexed (fn [index value]
                  (when (not= index omitted-index) value))
                collection))

;; Input

(def sampleInput "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(def input (slurp "src/inputs/day02"))

(defn parse [data]
  (->> data
       (str/split-lines)
       (map #(str/split % #" "))
       (map (fn [list] (map #(Integer/parseInt %) list)))))

;; Puzzle

(defn passes-rule-1 [report]
  ((some-fn ascending? descending?) report))

(defn safe-diff? [a b]
  (let [min 1 max 3 diff (abs-diff a b)]
    (and (>= diff min) (<= diff max))))

(defn passes-rule-2 [report]
  (pairwise-compare safe-diff? report))

(defn is-report-safe [report]
  ((every-pred passes-rule-1 passes-rule-2) report))

(defn is-report-tolerable [report]
  (or (is-report-safe report)
      (some (fn [omitted-index]
              (is-report-safe
               (omit-by-index omitted-index report)))
            (range (count report)))))

(defn solve1 [reports]
  (->> reports (filter is-report-safe) (count)))

(defn solve2 [reports]
  (->> reports (filter is-report-tolerable) (count)))

(def sampleReports (parse sampleInput))
(def reports (parse input))

(solve1 sampleReports)
(solve1 reports)

(solve2 sampleReports)
(solve2 reports)

