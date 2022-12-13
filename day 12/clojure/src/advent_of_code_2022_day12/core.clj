(ns advent-of-code-2022-day12.core)
(require '[clojure.string :as str])

(defn canMoveUp?
  ""
  [matrix current desired]
  (and (< -1 (:row desired) (count matrix))
       (< -1 (:col desired) (count (first matrix)))
       (>= (inc (:val (nth (nth matrix (:row current)) (:col current))))
           (:val (nth (nth matrix (:row desired)) (:col desired))))
       (or (= (:d (nth (nth matrix (:row desired)) (:col desired))) -1)
           (> (:d (nth (nth matrix (:row desired)) (:col desired)))
              (inc (:d (nth (nth matrix (:row current)) (:col current))))))))

(defn canMoveDown?
  ""
  [matrix current desired]
  (and (< -1 (:row desired) (count matrix))
       (< -1 (:col desired) (count (first matrix)))
       (>= (inc (:val (nth (nth matrix (:row desired)) (:col desired))))
           (:val (nth (nth matrix (:row current)) (:col current))))
       (or (= (:d (nth (nth matrix (:row desired)) (:col desired))) -1)
           (> (:d (nth (nth matrix (:row desired)) (:col desired)))
              (inc (:d (nth (nth matrix (:row current)) (:col current))))))))

(defn dijkstraUp
  ""
  ([matrix start stop] (dijkstraUp matrix (vector start) stop 0))
  ([matrix path stop distance]
   (let [currentPos (last path)
         newMatrix (assoc matrix
                          (:row currentPos) (assoc (nth matrix (:row currentPos))
                                                   (:col currentPos) (assoc (nth (nth matrix (:row currentPos)) (:col currentPos))
                                                                            :path path :d distance)))]
     (if (= stop currentPos)
       newMatrix
       (let [rightMatrix (let [desired {:row (:row currentPos) :col (inc (:col currentPos))}]
                           (if (canMoveUp? newMatrix currentPos desired)
                             (dijkstraUp newMatrix (conj path desired) stop (inc distance))
                             newMatrix))
             leftMatrix (let [desired {:row (:row currentPos) :col (dec (:col currentPos))}]
                          (if (canMoveUp? rightMatrix currentPos desired)
                            (dijkstraUp rightMatrix (conj path desired) stop (inc distance))
                            rightMatrix))
             upMatrix (let [desired {:row (dec (:row currentPos)) :col (:col currentPos)}]
                        (if (canMoveUp? leftMatrix currentPos desired)
                          (dijkstraUp leftMatrix (conj path desired) stop (inc distance))
                          leftMatrix))
             downDesired {:row (inc (:row currentPos)) :col (:col currentPos)}]
         (if (canMoveUp? upMatrix currentPos downDesired)
           (dijkstraUp upMatrix (conj path downDesired) stop (inc distance))
           upMatrix))))))

(defn dijkstraDown
  ""
  ([matrix start] (dijkstraDown matrix (vector start) 0))
  ([matrix path distance]
   (let [currentPos (last path)
         newMatrix (assoc matrix
                          (:row currentPos) (assoc (nth matrix (:row currentPos))
                                                   (:col currentPos) (assoc (nth (nth matrix (:row currentPos)) (:col currentPos))
                                                                            :path path :d distance)))]
     (if (= (int \a) (:val (nth (nth newMatrix (:row currentPos)) (:col currentPos))))
       newMatrix
       (let [rightMatrix (let [desired {:row (:row currentPos) :col (inc (:col currentPos))}]
                           (if (canMoveDown? newMatrix currentPos desired)
                             (dijkstraDown newMatrix (conj path desired) (inc distance))
                             newMatrix))
             leftMatrix (let [desired {:row (:row currentPos) :col (dec (:col currentPos))}]
                          (if (canMoveDown? rightMatrix currentPos desired)
                            (dijkstraDown rightMatrix (conj path desired) (inc distance))
                            rightMatrix))
             upMatrix (let [desired {:row (dec (:row currentPos)) :col (:col currentPos)}]
                        (if (canMoveDown? leftMatrix currentPos desired)
                          (dijkstraDown leftMatrix (conj path desired) (inc distance))
                          leftMatrix))
             downDesired {:row (inc (:row currentPos)) :col (:col currentPos)}]
         (if (canMoveDown? upMatrix currentPos downDesired)
           (dijkstraDown upMatrix (conj path downDesired) (inc distance))
           upMatrix))))))

(defn -main
  "Main function."
  []
  (let [input (mapv #(into [] (char-array (str/replace % #"\p{C}" ""))) (str/split-lines (slurp "../input.txt")))
        preparedInput (mapv (fn [row] (mapv (fn [val] {:path nil :d -1 :val (cond (= val \S) (int \a)
                                                                                  (= val \E) (int \z)
                                                                                  :else (int val))}) row)) input)
        startCoord (reduce-kv (fn [outerCurrent rowIndex row]
                                (reduce-kv (fn [current colIndex val]
                                             (if (= val \S) {:row rowIndex :col colIndex} current)) outerCurrent row)) {:row -1 :col -1} input)
        stopCoord (reduce-kv (fn [outerCurrent rowIndex row]
                               (reduce-kv (fn [current colIndex val]
                                            (if (= val \E) {:row rowIndex :col colIndex} current)) outerCurrent row)) {:row -1 :col -1} input)]
    (println "Distance Start to End:  " (:d (nth (nth (dijkstraUp preparedInput startCoord stopCoord) (:row stopCoord)) (:col stopCoord))))
    (println "Smallest Distance to a: " (apply min (filter #(not= % -1) (reduce (fn [outerCurrent row]
                                                                                  (reduce (fn [innerCurrent val]
                                                                                            (if (= (:val val) (int \a))
                                                                                              (conj innerCurrent (:d val))
                                                                                              innerCurrent))
                                                                                          outerCurrent row))
                                                                                (list) (dijkstraDown preparedInput stopCoord)))))))
