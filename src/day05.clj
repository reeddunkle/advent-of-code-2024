(ns day05 (:require
           [clojure.string :as str]
           [clojure.math :as math]))

(defn get-middle-index [list]
  (let [length (count list)
        middleIndex (math/floor-div length 2)]
    middleIndex))

(defn parse-ints [coll]
  (map #(Integer/parseInt %) coll))

(defn relevant-rule? [rule update]
  (every? (fn [[ruleA ruleB]] (and (contains? update ruleA) (contains? update ruleB))) [rule]))

(defn valid-update? [update rules]
  (let [rule-set (set rules)]
    (reduce (fn [[valid? seen] n]
              (if (not valid?)
                [false seen] ;; Short-circuit
                (let [remaining (set (rest update))
                      valid-next? (every? (fn [next-n]
                                            (or (rule-set [n next-n])
                                                (rule-set [next-n n])))
                                          remaining)]
                  [valid-next? (conj seen n)])))
            [true #{}]
            update)
    first))

(def rules1 '((47 53) (97 13) (97 61) (97 47) (75 29) (61 13) (75 53) (29 13) (97 29) (53 29) (61 53) (97 53) (61 29) (47 13) (75 47) (97 75) (47 61) (75 61) (47 29) (75 13) (53 13)))

(println rules1)

(def pass-update '(75 47 61 53 29))
(def fail-update '(75 97 47 61 53))

(def fail-rules (filter (fn [rule] (relevant-rule? rule (set fail-update))) rules1))

(def pass-rules (filter (fn [rule] (relevant-rule? rule (set pass-update))) rules1))

(println (valid-update? fail-update fail-rules))
(println (valid-update? pass-update pass-rules))

(defn solve1 [data]
  (let [[_rules _ _updates] (->> (str/split-lines data)
                                 (partition-by str/blank?))
        rules (->> _rules
                   (map (fn [line]
                          (parse-ints (str/split line #"\|")))))
        updates (->> _updates
                     (map (fn [line]
                            (parse-ints (str/split line #",")))))]
    (println rules)
    (println updates)
    (->> updates
         (filter (fn [update]
                   (let [filtered-rules (filter #(relevant-rule? % (set update)) rules)]
                     (valid-update? update filtered-rules))))
         (map #(nth % (get-middle-index %)))
         (reduce +))))

(solve1 (slurp "src/inputs/day05-p1"))

