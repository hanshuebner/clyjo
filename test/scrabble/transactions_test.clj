(ns scrabble.transactions-test
  (:require [scrabble.core :refer :all]
            [scrabble.transactions :as transactions]
            [clojure.test :refer :all]
            [slingshot.test :refer :all]
            [clojure.java.io :as io]))

;; FIXME: me.raynes.fs

(defn make-logfile-name []
  (format "%s/transaction-log-%s-%s.log"
          (System/getProperty "java.io.tmpdir")
          (System/currentTimeMillis)
          (long (rand 0x100000000))))

(defn fixture [f]
  (let [logfile (io/file (make-logfile-name))]
    (when (.exists logfile)
      (io/delete-file logfile))
    (transactions/open-logger logfile)
    (make-bank)
    (f)
    (transactions/close-logger)
    (when (.exists logfile)
      (io/delete-file logfile))))

(use-fixtures :each fixture)

(deftest basics
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

(deftest bonds
  (let [donald (make-account! "donald")
        scrooge (make-account! "scrooge")]
    (is (thrown? Exception
                 (borrow! scrooge donald 10)))
    (deposit! scrooge 10000)
    (borrow! scrooge donald 10)
    (is (= (:scrabble.core/balance @donald) 10))
    (is (= (:scrabble.core/balance @scrooge 9990)))
    (is (thrown? Exception
                 (borrow! scrooge donald 10000)))
    (is (thrown? Exception
                 (repay! donald scrooge 20)))
    (is (thrown? Exception
                 (repay! donald scrooge 5)))
    (repay! donald scrooge 10)
    (is (thrown? Exception
                 (repay! donald scrooge 10)))
    (is (empty? (:scrabble.core/bonds @scrooge)))))
