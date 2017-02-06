(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [ac base exp]
                 (if (zero? exp)
                   ac
                   (recur (* ac base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (cond
   (empty? a-seq) nil
    (= 1 (count a-seq)) (first a-seq)
    :else (recur (rest a-seq))
   )
)

(defn seq= [a-seq b-seq]
  (cond
  (and (empty? a-seq) (empty? b-seq)) true
  (= a-seq b-seq) (recur (rest a-seq) (rest b-seq))
   :else false
  )
)

(defn find-first-index [pred a-seq]
  (loop [n 0
         s a-seq]
    (cond
     (empty? s) nil
     (pred (first s)) n
     :else (recur (inc n) (rest s))
    )
))

(defn avg [a-seq]
  (loop [n 0
         jako 0
         s a-seq]
    (cond
      (empty? s) (/ n jako)
     :else (recur (+ n (first s)) (inc jako) (rest s))))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn parity [a-seq]
  (loop [stri #{}
         s a-seq]
    (cond
      (empty? s) stri
     :else (recur (toggle stri (first s))  (rest s))))
)

(defn fast-fibo [n]
  (loop [x1 0
         x2 1
         nt 2]
    (cond
     (= 1 n) 1
     (= 0 n) 0
     (= nt n) (+ x1 x2)
     :else (recur x2 (+ x2 x1) (inc nt)))
    )
)

(defn cut-at-repetition [a-seq]
(loop [st #{}
         a-seq a-seq
         acc []]
      (cond
        (empty? a-seq) acc
        (st (first a-seq)) acc
        :else (recur (conj st (first a-seq)) (rest a-seq) (conj acc (first a-seq)))
)))

