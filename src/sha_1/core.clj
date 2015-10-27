(ns sha-1.core
  (:require [clojure.string :refer [join]]))

(defn str->codes [s]
  (map int s))

(defn bstring [i]
  (Integer/toBinaryString (bit-and i 0xFF)))

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

;; Byte Arrays

(defn msg->bytes [msg]
  (byte-array (map (comp byte int) msg)))

(defn padding-bytes
  "Add appropriate number of 0 bytes to bring message into congruence with 448
   mod 512 (56 mod 64 bytes). Additionally, the first bit of this padding sequence
   should be set 1 to separate original message from padding."
  [msg-bytes]
  (let [mod-offset (- 56 (mod (count msg-bytes) 64))
        bytes-needed (if (< mod-offset 0)
                       (+ mod-offset 64)
                       mod-offset)]
    (byte-array
     (concat
      [0x80]
      (take (dec bytes-needed) (repeat 0))))))

(defn long-empty-bytes-count [n]
  (int (/ (Long/numberOfLeadingZeros n) 8)))

(defn long->bytes
  "Chunks 64-bit long into appropriate number of bytes"
  [n]
  (loop [n n
         bytes (list)]
    (if (= 0 n)
      bytes
      (recur (bit-shift-right n 8) (conj bytes (bit-and n 0xF))))))

(defn msg-length-bytes [msg]
  (let [pad-count (long-empty-bytes-count (count msg))
        empty-bytes (take pad-count (repeat 0))
        filled-bytes (long->bytes (count msg))]
    (byte-array (concat empty-bytes filled-bytes))))

(defn inspect-byte-array
  "Util function for manually inspecting byte arrays as strings"
  [barray]
  (->> barray
       (map (partial padded 8))
       (join " ")))

(defn with-padding-bytes [m-bytes] (byte-array (concat m-bytes (padding-bytes m-bytes))))
(defn with-message-length-bytes [m-bytes] (byte-array (concat m-bytes (msg-length-bytes m-bytes))))

(defn msg->prepped-barray
  "takes incoming string message through various steps needed to
   convert it to a mod-512 byte array with appropriate padding and
   meessage length appended"
  [msg]
  (-> msg
      (msg->bytes)
      (concat (padding-bytes msg))
      (concat (msg-length-bytes msg))
      (byte-array)))
