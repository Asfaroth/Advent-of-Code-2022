(ns advent-of-code-2022-day01.core)
(require '[clojure.string :as str])

(defn getTopX
  "Gets the sum of the top x elves. Intentionally without using a sort algorithm!"
  ([totals x] (getTopX (map-indexed (fn [i x] {:id i :value x}) totals) x 0))
  ([totals x i] (if (= x i)
                  0
                  (let [biggest (apply max-key :value totals)]
                    (+ (:value biggest)
                       (getTopX (remove #(= (:id biggest) (:id %)) totals) x (+ i 1)))))))

(defn -main
  "Main function."
  []
  (let [totals (map (fn [x] (reduce + (map #(Integer/parseInt %) (str/split-lines x)))) (str/split (slurp "../input.txt") #"\n\n"))]
    (println "Top Elf: " (apply max totals))
    (println "Top 3:   " (getTopX totals 3))))
