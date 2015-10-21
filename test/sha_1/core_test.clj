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
