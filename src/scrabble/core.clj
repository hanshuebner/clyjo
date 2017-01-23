(ns scrabble.core
  (:require [clojure.spec :as s]
            [scrabble.transactions :refer [deftx pref-set pref]]))

(def accounts (pref {}))

(defn get-account [name]
  (get @accounts name))

(defn account-valid? [account]
  (not (neg? (::balance account))))

(s/def ::account (s/and map?))

(deftx make-account! [name]
  {:pre [(not (get-account name))]}
  (pref-set accounts (assoc @accounts name (pref {::balance 0}
                                                 :validator account-valid?))))

(deftx deposit! [account amount]
  {:pre [(s/conform ::account account)
         (pos? amount)]}
  (pref-set account (update @account ::balance + amount)))

(deftx withdraw! [account amount]
  {:pre [(s/conform ::account account)
         (pos? amount)]}
  (pref-set account (update @account ::balance - amount)))

(deftx transfer! [from to amount]
  {:pre [(and (s/conform ::account from)
              (s/conform ::account to))]}
  (withdraw! from amount)
  (deposit! to amount))
