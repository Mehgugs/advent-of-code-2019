#parsing input
(def nums (->> (slurp "01.input") (string/trim) (string/split "\n") (map (comp scan-number string/trim))))

#calculate fuel used
(defn compute-fuel [x]
    (-> x (/ 3) (math/floor) (- 2)))

#calculate full fuel amount
(defn full-fuel [a x]
    (def fuel (compute-fuel x))
    (if (< fuel 0)
        a
        (full-fuel (+ fuel a) fuel)))

#part one
(def results-1 (map compute-fuel nums))
(printf "Part 1: %f\n" (sum results-1))

#part two
(def results-2 (map (partial full-fuel 0) nums))
(printf "Part 2: %f\n" (sum results-2))