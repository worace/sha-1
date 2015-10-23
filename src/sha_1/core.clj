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

(defn msg->bytes [msg]
  (byte-array (map (comp byte int) msg)))

(def block-bit-length 512)
(def block-byte-length (/ 8 block-bit-length))
(def message-length-bits 64)
(def message-length-bytes (/ 8 64))

(defn padding-bytes [msg-bytes]
  (let [mod-offset (- 56 (mod (count msg-bytes) 64))
        bytes-needed (if (< mod-offset 0)
                       (+ mod-offset 64)
                       mod-offset)]
    (byte-array
     (take bytes-needed (repeat 0)))))
