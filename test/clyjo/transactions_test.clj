(ns clyjo.transactions-test
  (:require [clyjo.test.bank :as bank]
            [clyjo.transactions :as transactions]
            [clojure.test :refer :all]
            [slingshot.test :refer :all]
            [clojure.java.io :as io]))

;; FIXME: me.raynes.fs

(defn make-logfile-name []
  (format "%s/transaction-log-%s-%s.log"
          (System/getProperty "java.io.tmpdir")
          (System/currentTimeMillis)
          (long (rand 0x100000000))))

(def ^:dynamic *transaction-log-filename*)

(defn fixture [f]
  (binding [*transaction-log-filename* (io/file (make-logfile-name))]
    (when (.exists *transaction-log-filename*)
      (io/delete-file *transaction-log-filename*))
    (transactions/open-store *transaction-log-filename*)
    (bank/make-bank)
    (f)
    (transactions/close-store)
    (when (.exists *transaction-log-filename*)
      (io/delete-file *transaction-log-filename*))))

(use-fixtures :each fixture)

(deftest basics
  (let [a (bank/make-account! "a")
        b (bank/make-account! "b")]
    (is (= (:clyjo.test.bank/balance @a) 0))
    (is (= (:clyjo.test.bank/balance @b) 0))
    (bank/deposit! a 10)
    (is (= (:clyjo.test.bank/balance @a) 10))
    (bank/transfer! a b 10)
    (is (= (:clyjo.test.bank/balance @a) 0))
    (is (= (:clyjo.test.bank/balance @b) 10))
    (is (thrown? Exception
                 (bank/transfer! a b 10)))
    (bank/set-overdraft-limit! a 10)
    (bank/transfer! a b 10)
    (is (= (:clyjo.test.bank/balance @a) -10))
    (is (= (:clyjo.test.bank/balance @b) 20))
    (is (thrown? Exception
                 (bank/transfer! a b 10)))))

(deftest bonds
  (let [donald (bank/make-account! "donald")
        scrooge (bank/make-account! "scrooge")]
    (is (thrown? Exception
                 (bank/borrow! scrooge donald 10)))
    (bank/deposit! scrooge 10000)
    (bank/borrow! scrooge donald 10)
    (is (= (:clyjo.test.bank/balance @donald) 10))
    (is (= (:clyjo.test.bank/balance @scrooge 9990)))
    (is (thrown? Exception
                 (bank/borrow! scrooge donald 10000)))
    (is (thrown? Exception
                 (bank/repay! donald scrooge 20)))
    (is (thrown? Exception
                 (bank/repay! donald scrooge 5)))
    (bank/repay! donald scrooge 10)
    (is (thrown? Exception
                 (bank/repay! donald scrooge 10)))
    (is (empty? (:clyjo.test.bank/bonds @scrooge)))
    (is (= (:clyjo.test.bank/balance @donald) 0))
    (is (= (:clyjo.test.bank/balance @scrooge 10000)))))

(deftest restore
  (let [donald (bank/make-account! "donald")
        scrooge (bank/make-account! "scrooge")]
    (bank/deposit! scrooge 10000)
    (bank/borrow! scrooge donald 10))
  (transactions/open-store *transaction-log-filename*)
  (let [donald (bank/get-account "donald")
        scrooge (bank/get-account "scrooge")]
    (is (= (:clyjo.test.bank/balance @donald) 10))
    (is (= (:clyjo.test.bank/balance @scrooge 9990)))))
