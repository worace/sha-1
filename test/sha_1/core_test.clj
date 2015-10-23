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

(deftest test-msg-bytes
  (is (= 2 (count (msg->bytes "ab")))))

(deftest test-padding-length
  (is (= 54 (count (padding-bytes (msg->bytes "ab"))))))

(deftest test-padding-longer-msg
  ; 520 bits, so wraps around
  (let [msg (apply str (take 65 (repeat "a")))
        mb (msg->bytes msg)]
    (is (= 55 (count (padding-bytes mb))))))

(deftest test-padding-448-512-msg
  (let [msg (apply str (take 57 (repeat "a")))
        mb (msg->bytes msg)]
    (is (= 63 (count (padding-bytes mb))))))
