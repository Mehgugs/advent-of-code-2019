(def input (->> (slurp "02.input") (string/trim) (string/split ",") (map (comp scan-number string/trim))))

(def comp-state @{
    :reset (fn [state]
            (put state :intcodes (array/slice (state :initial)))
            (put state :position 0))
    :input (fn [{:intcodes I} noun verb]
            (put I 1 noun)
            (put I 2 verb))
    :output (fn [{:intcodes I}] (I 0))
    #ops
    :add (fn [state]
            (def {:intcodes I :position P} state)
            (put I (I (+ P 2)) (+ (I (I P)) (I (I (+ P 1)))))
            (put state :position (+ P 3)))
    :mult (fn [state]
            (def {:intcodes I :position P} state)
            (put I (I (+ P 2)) (* (I (I P)) (I (I (+ P 1)))))
            (put state :position (+ P 3)))
    :seek (fn [state]
            (def {:intcodes I :position P} state)
            (put state :position (+ P 1))
            (I P))
    :run (fn [state]
            (while true
            (case (:seek state)
                1 (:add state)
                2 (:mult state)
                99 (break))))})

(defn new-state [intcodes]
    (table/setproto @{:intcodes (array/slice intcodes) :initial intcodes :position 0} comp-state))

(def result (new-state input))

(:input result 12 2)
(:run result)
(printf "%f\n" (:output result))

(:reset result)

(for i 0 (- (length input) 1)
    (for j 0 (- (length input) 1)
        (:input result i j)
        (:run result)
        (if (= (:output result) 19690720)
            (do (printf "%f\n" (+ (* 100 i) j)) (:reset result))
            (:reset result))))