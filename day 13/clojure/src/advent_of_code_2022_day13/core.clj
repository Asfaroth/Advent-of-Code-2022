(ns advent-of-code-2022-day13.core)
(require '[clojure.string :as str]
         '[clojure.data.json :as json])

(defn packetsRightOrder?
  "Checks whether two given packets are in the same order. If the subList flag is given, no result results in nil as return value instead of a boolean."
  ([packet1 packet2] (packetsRightOrder? packet1 packet2 false))
  ([packet1 packet2 subList?]
   (cond (and (empty? packet1)
              (empty? packet2)) (if subList? nil true)
         (empty? packet1) true
         (empty? packet2) false  ;; need a boolean type here instead of (seq packet2)
         :else (let [first1 (first packet1)
                     first2 (first packet2)]
                 (cond (and (number? first1)
                            (number? first2)) (if (= first1 first2)
                                                (recur (drop 1 packet1) (drop 1 packet2) subList?)
                                                (< first1 first2))
                       (and (vector? first1)
                            (vector? first2)) (let [subComparison (packetsRightOrder? first1 first2 true)]
                                                (if (nil? subComparison)
                                                  (recur (drop 1 packet1) (drop 1 packet2) subList?)
                                                  subComparison))
                       (number? first1) (recur (vector (vector first1) (into [] (drop 1 packet1))) packet2 subList?)
                       :else (recur packet1 (vector (vector first2) (into [] (drop 1 packet2))) subList?))))))

(defn sortPacket
  "Sorts a given packet into the given collection and returns it."
  [collection packet]
  (cond (empty? collection) (vector packet)
        (packetsRightOrder? (first collection) packet) (into (vector (first collection)) (sortPacket (drop 1 collection) packet))
        :else (into (vector packet) collection)))

(defn -main
  "Main function."
  []
  (let [input  (mapv #(mapv json/read-str (str/split-lines %)) (str/split (slurp "../input.txt") #"\n\n"))
        rightOrder? (mapv #(packetsRightOrder? (first %) (second %) false) input)
        firstDivider [[2]]
        secondDivider [[6]]
        input2 (conj (conj (reduce #(into %1 %2) [] input) firstDivider) secondDivider)
        sortedInput (reduce #(sortPacket %1 %2) [] input2)]
    (println "Sum of wrong ordered indexes: " (reduce-kv #(if %3 (+ (inc %2) %1) %1) 0 rightOrder?))
    (println "Product of divider indexes:   " (reduce-kv #(if (or (= %3 firstDivider) (= %3 secondDivider)) (* %1 (inc %2)) %1) 1 sortedInput))))
