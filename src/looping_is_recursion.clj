(ns looping-is-recursion)

(defn power [base exp]
  (loop [acc 1
         n exp]
    (if (zero? n)
      acc
      (recur (* acc base) (dec n)))))

(defn last-element [a-seq]
  (cond
   (empty? a-seq) nil
   (and (not (empty? a-seq))
        (empty? (rest a-seq))) (first a-seq)
   :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1)
        (empty? seq2)) true
   (or (empty? seq1)
       (empty? seq2)) false
   (not (= (first seq1)
           (first seq2))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         l a-seq]
    (cond
     (empty? l) nil
     (pred (first l)) n
     :else (recur (inc n) (rest l)))))

(defn avg [a-seq]
  (loop [total 0
         nums 0
         lst a-seq]
    (cond
     (empty? lst) (if (zero? nums) 0 (/ total nums))
     :else (recur (+ total (first lst))
                  (inc nums)
                  (rest lst)))))

(defn parity [a-seq]
  (defn toggle [a-set elem]
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))
  (loop [odds #{}
         lst a-seq]
    (if (empty? lst)
      odds
      (recur (toggle odds (first lst)) (rest lst)))))

(defn fast-fibo [n]
  (loop [n-1 0
         acc 1
         cnt n]
    (if (zero? cnt)
      n-1
      (recur acc (+ acc n-1) (dec cnt)))))

(defn cut-at-repetition [a-seq]
  (loop [lst a-seq
         acc '()]
    (cond
     (empty? lst) (reverse acc)
     (some #(= % (first lst)) acc) (reverse acc)
     :else (recur (rest lst) (cons (first lst) acc)))))

