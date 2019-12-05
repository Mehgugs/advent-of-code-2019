(def START 248345)
(def END 746315)

(def S 2)
(def E 7)

(defn compute-1! []
    (var res 0)
    (for i S (+ 1 E)
        (for j i 10
        (for k j 10
        (for l k 10
        (for m l 10
        (for n m 10
        (def x (+ (* i 100_000) (* j 10_000) (* k 1_000) (* l 100) (* m 10) n))
        (if
            (and
                (<= START x)
                (<= x END)
                (or (= i j) (= j k) (= k l) (= l m) (= m n)))
            (++ res))))))))
    res)

(defn count-map [& digits]
    (def out @{})
    (loop [d :in digits]
        (put out d (+ 1 (get out d 0))))
    out)

(defn compute-2! []
    (var res 0)
    (for i S (+ 1 E)
        (for j i 10
        (for k j 10
        (for l k 10
        (for m l 10
        (for n m 10
        (def x (+ (* i 100_000) (* j 10_000) (* k 1_000) (* l 100) (* m 10) n))
        (if
            (and
                (<= START x)
                (<= x END)
                (or (= i j) (= j k) (= k l) (= l m) (= m n))
                (find (fn [x] (= x 2)) (values (count-map i j k l m n))))
            (++ res))))))))
    res)

(print (compute-1!) " " (compute-2!))