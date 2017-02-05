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
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

