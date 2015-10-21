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

(defn padded-msg-bits [msg]
  (->> msg
       (map int)
       (map (partial padded 8))
       (apply str)))

(defn congruence-bits [msg-bits]
  (apply str (repeat (- 448 (count msg-bits)) "0")))

(defn prep-message [msg]
  (let [msg-bits (str (padded-msg-bits msg) "1")]
    (apply str
           msg-bits
           (congruence-bits msg-bits)
           (padded 64 (count msg)))))
