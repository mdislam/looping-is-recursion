(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn[acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc n]
                 (if (empty? acc)
                   n
                   (recur (rest acc) (first acc))))]
        (helper (rest a-seq) (first  a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (or (empty? seq1) (empty? seq2)) false
        (not= (first seq1) (first seq2)) false
        :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [my-seq a-seq
  	     index 0]
  	(cond
  		(empty? my-seq) nil
  		(pred (first my-seq)) index
  		:else (recur (rest my-seq) (inc index)))))

(defn avg [a-seq]
  (loop [sum 0
         my-seq a-seq
         num-elem (count a-seq)]
    (cond (= num-elem 0) sum
          (empty? my-seq) (/ sum num-elem)
          :else (recur (+ sum (first my-seq)) (rest my-seq) num-elem))))


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [my-seq a-seq
         my-set (set [])]
    (cond
     (empty? my-seq) my-set
     :else (recur (rest my-seq) (toggle my-set (first my-seq))))))

(defn fast-fibo [n]
  (loop [second-num 1
         first-num 0
         third-num 2
         n n]
    (cond
     (< n 2) n
     (= third-num n) (+ second-num first-num)
     :else (recur (+ second-num first-num) second-num (+ third-num 1) n))))

(defn cut-at-repetition [a-seq]
  (loop [k []
  	     my-seq a-seq]
  	(cond
  		(empty? my-seq) k
  		(contains? (set k) (first my-seq)) k
  		:else (recur (conj k (first my-seq)) (rest my-seq)))))

