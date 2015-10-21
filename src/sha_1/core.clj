(ns sha-1.core)

(defn str->codes [s]
  (map int s))

(defn bstring [i]
  (Integer/toBinaryString i))

(defn padded [bit-count i]
  (let [s (bstring i)]
    (apply str
           (concat (repeat (- bit-count (count s)) "0")
                   s))))
