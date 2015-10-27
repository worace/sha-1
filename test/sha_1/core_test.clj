(ns sha-1.core-test
  (:require [clojure.test :refer :all]
            [sha-1.core :refer :all]))


(deftest test-chars
  (is (= [\a \b] (seq "ab"))))

(deftest test-ascii-codes
  (is (= [97 98] (str->codes "ab"))))

(deftest test-padding-codes
  (is (= "00000011" (padded 8 3))))

;; string "ab" should give
;; 97,98 binary
;; + "1"
;; + diff to make congruent mod 512
;; + message length in characters padded to 64 bits
;; or:
;; "11000011" + "100010" + "1" + ("0" * 443) + (64-bit padded 2)
(def prepped-ab
  (str (padded 8 (int \a))
       (padded 8 (int \b))
       "1"
       (apply str (repeat 431 "0"))
       (padded 64 2)))

(deftest test-congruence-bits
  (is (= (apply str (repeat 443 "0"))
         (congruence-bits "11111"))))

(deftest test-padded-message
  (is (= 512 (count prepped-ab)))
  (is (= (count prepped-ab) (count (prep-message "ab"))))
  (is (= prepped-ab (prep-message "ab"))))

;; byte arrays

(deftest test-msg-bytes
  (is (= 2 (count (msg->bytes "ab")))))

(deftest test-padding-length
  (testing "pads short message to 56 total bytes, leaving 8 bytes in
            same 512-bit block for message length. Additionally, leading
            padding bit should be 1 to distinguish padding from message"
    (is (= 54 (count (padding-bytes "ab"))))
    (is (= -0x80 (first (padding-bytes "ab"))))))

(deftest test-padding-longer-msg
  ; 520 bits, so wraps around
  (testing "pads message longer than 512 bits to be congruent with 448
            mod 512, leaving 8 bytes for message length in next block"
    (let [msg (apply str (take 65 (repeat "a")))
          mb (msg->bytes msg)]
      (is (= 55 (count (padding-bytes mb))))
      (is (= -0x80 (first (padding-bytes mb)))))))

(deftest test-padding-448-512-msg
  (testing "message longer that is greater than 448 mod 512 bits still has to
            pad to 448 congruence in next block since there aren't enough bits
            left in current block for 64-bit message-length encoding"
    (let [msg (apply str (take 57 (repeat "a")))
          mb (msg->bytes msg)]
      (is (= 63 (count (padding-bytes mb))))
      (is (= "10000000" (bstring (first (padding-bytes mb)))))
      (is (= -0x80 (first (padding-bytes mb)))))))

(deftest test-msg-length-bytes
  (testing "generates byte-array for encoding message length as 64-bit long"
    (is (= 8 (count (msg-length-bytes "a"))))
    (is (= (map bstring (byte-array (concat (take 7 (repeat 0)) [1])))
           (map bstring (msg-length-bytes "a"))))))

(deftest test-prepping-msg-as-byte-array
  (testing "preps message by converting to bytes, adding padding separator bit,
            adding congruence padding, and adding message length bits"
    (let [m "ab"
          prepped (msg->prepped-barray "ab")]
      (is (= 64 (count prepped)))
      (is (= (int \a) (aget prepped 0)))
      (is (= (int \b) (aget prepped 1)))
      (is (= 2 (last prepped)))
      (is (= -0x80 (aget prepped 2))))))
