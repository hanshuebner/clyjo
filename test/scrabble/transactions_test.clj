(ns scrabble.transactions-test
  (:require [scrabble.core :refer :all]
            [clojure.test :refer :all]))

(deftest test-basics
  (clear-all-accounts!)
  (let [a (make-account! "a")
        b (make-account! "b")]
    (is (= (:scrabble.core/balance @a) 0))
    (is (= (:scrabble.core/balance @b) 0))
    (deposit! a 10)
    (is (= (:scrabble.core/balance @a) 10))
    (transfer! a b 10)
    (is (= (:scrabble.core/balance @a) 0))
    (is (= (:scrabble.core/balance @b) 10))
    (is (thrown? Exception
                 (transfer! a b 10)))
    (set-overdraft-limit! a 10)
    (transfer! a b 10)
    (is (= (:scrabble.core/balance @a) -10))
    (is (= (:scrabble.core/balance @b) 20))
    (is (thrown? Exception
                 (transfer! a b 10)))))
