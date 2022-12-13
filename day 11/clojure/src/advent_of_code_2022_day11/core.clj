(ns advent-of-code-2022-day11.core)
(require '[clojure.string :as str])

(defn calcRounds
  "Calculates the rounds and automatically shortens the worry levels to keep them small."
  ([monkeys stop divisor] (calcRounds 0 stop monkeys 0 divisor (reduce * (map :testDivisor monkeys))))
  ([roundNr stop monkeys monkeyNr divisor moduloShorter]
   (if (>= roundNr stop)
     monkeys
     (let [currentMonkey (nth monkeys monkeyNr)
           newItems (map #(mod
                           (quot ((:function currentMonkey) %) divisor) moduloShorter) ;; works because of congruence
                         (:items currentMonkey))
           successItems (filter #(= (mod % (:testDivisor currentMonkey)) 0) newItems)
           failItems (filter #(> (mod % (:testDivisor currentMonkey)) 0) newItems)
           newMonkeyNr (mod (inc monkeyNr) (count monkeys))]
       (recur (if (= newMonkeyNr 0) (inc roundNr) roundNr)
              stop
              (assoc monkeys
                     monkeyNr (assoc (nth monkeys monkeyNr)
                                     :items (vector)
                                     :throws (+ (:throws (nth monkeys monkeyNr)) (count newItems)))
                     (:testSuccess currentMonkey) (assoc (nth monkeys (:testSuccess currentMonkey))
                                                         :items (flatten (conj (:items (nth monkeys (:testSuccess currentMonkey))) successItems)))
                     (:testFail currentMonkey) (assoc (nth monkeys (:testFail currentMonkey))
                                                      :items (flatten (conj (:items (nth monkeys (:testFail currentMonkey))) failItems))))
              newMonkeyNr
              divisor
              moduloShorter)))))

(defn -main
  "Main function."
  []
  (let [input (map #(drop 1 (str/split-lines %)) (str/split (slurp "../input.txt") #"\n\n"))
        monkeys (mapv (fn [rawMonkey] {:items (mapv #(biginteger (Integer/parseInt (str/replace % #"," "")))
                                                    (str/split
                                                     (last (str/split (first rawMonkey) #": "))
                                                     #" "))
                                       :function (let [split (take-last 2 (str/split (second rawMonkey) #" "))
                                                       operand (first split)
                                                       number (second split)]
                                                   (if (= operand "+")
                                                     (if (= number "old")
                                                       (fn [summand] (+ summand summand))
                                                       (fn [summand] (+ (Integer/parseInt number) summand)))
                                                     (if (= number "old")
                                                       (fn [factor] (* factor factor))
                                                       (fn [factor] (* (Integer/parseInt number) factor)))))
                                       :testDivisor (Integer/parseInt (last (str/split (nth rawMonkey 2) #" ")))
                                       :testSuccess (Integer/parseInt (last (str/split (nth rawMonkey 3) #" ")))
                                       :testFail (Integer/parseInt (last (str/split (nth rawMonkey 4) #" ")))
                                       :throws 0}) input)]
    (println "   20 rounds divisor 3: " (reduce * (take 2 (sort > (map :throws (calcRounds monkeys 20 3))))))
    (println "10000 rounds divisor 1: " (reduce * (take 2 (sort > (map :throws (calcRounds monkeys 10000 1))))))))
