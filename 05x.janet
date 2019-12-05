(def input (->> (slurp "05.input") (string/trim) (string/split ",") (map (comp scan-number string/trim))))

(def op-parser (peg/compile
    ~{
        :main (* (+ (if (> 2 1) :R) (constant [])) (/ (capture (some 1)) ,(fn [x] (scan-number x))))
        :R (+ (/ (not (> 2 1)) []) (/ (* '1 :R) ,(fn [x y] (tuple ;y (scan-number x)))))}))

(defmacro method [obj name args & body]
    ~(put ,obj ,name (fn ,(symbol (string name)) ,args ,;body)))

(def computer @{})

(method computer :reset [state]
    (put state :intcodes (array/slice (state :initial)))
    (put state :position 0))

## reading routines ##
(method computer :read [state]
    (def {:intcodes I :position P} state)
    (put state :position (+ P 1))
    (I P))

(method computer :seek [state]
    (def bundle (peg/match op-parser (string (:read state))))
    [(bundle 1) (bundle 0)])

(method computer :read-args [state arity modes &opt dft]
    (def out @[])
    (loop [i :range [0 arity]]
        (def val (:read state))
        (array/push out
            (case (get modes i (if dft (get dft i 0) 0))
                0 (get-in state [:intcodes val])
                1 val)))
    out)

## opcodes ##
(method computer :1 [state modes]
    (def {:intcodes I :position P} state)
    (def [A B out] (:read-args state 3 modes [0 0 1]))
    (put I out (+ A B))
    true)

(method computer :2 [state modes]
    (def {:intcodes I :position P} state)
    (def [A B out] (:read-args state 3 modes [0 0 1]))
    (put I out (* A B))
    true)

(method computer :3 [state modes]
    (def {:intcodes I :position P} state)
    (def [out] (:read-args state 1 modes [1]))
    (put I out (yield :i))
    true)

(method computer :4 [state modes]
    (def [out] (:read-args state 1 modes [0]))
    (yield out)
    true)

(method computer :5 [state modes]
    (def [z j] (:read-args state 2 modes [0 0]))
    (if (not (zero? z)) (put state :position j))
    true)

(method computer :6 [state modes]
    (def [z j] (:read-args state 2 modes [0 0]))
    (if (zero? z) (put state :position j))
    true)

(method computer :7 [state modes]
    (def {:intcodes I :position P} state)
    (def [A B out] (:read-args state 3 modes [0 0 1]))
    (put I out (if (< A B) 1 0))
    true)

(method computer :8 [state modes]
    (def {:intcodes I :position P} state)
    (def [A B out] (:read-args state 3 modes [0 0 1]))
    (put I out (if (= A B) 1 0))
    true)

(method computer :99 [_ _] false)

(method computer :run [state]
    (fiber/new (fn []
        (while true
            (def [op param-modes] (:seek state))
            (if (not ((keyword op) state param-modes))
                (break))))))

(method computer :run-completely [state]
    (def f (:run state))
    (var req nil)
    (var input nil)
    (while (not= (fiber/status f) :dead)
        (set req (resume f input))
        (case req
            :i (do (prin "input: ") (set input (-> (:read stdin :line) string/trim scan-number)))
            nil (print "Done!")
            (printf "output: %i\n" req)))
    req)

(defn make-computer! [intcodes]
    (table/setproto @{:intcodes (array/slice intcodes) :initial intcodes :position 0} computer))

(def result (make-computer! input))
(:run-completely result)