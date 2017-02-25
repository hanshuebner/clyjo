(ns clyjo.test.bank
  (:require [clojure.spec :as s]
            [clyjo.defn-specs]
            [clyjo.transactions :refer [deftx pref-set palter pref]]
            [slingshot.slingshot :refer [throw+]]))

(defn within-limit? [{:keys [::balance ::overdraft-limit]
                      :or {overdraft-limit 0}}]
  (>= balance (- overdraft-limit)))

(defn bond-ref? [thing]
  (and (instance? clyjo.transactions.PersistentRef thing)
       (s/valid? ::bond @thing)))

(s/def ::account (s/and (s/keys :req [::name
                                      ::balance
                                      ::bonds]
                                :optional [::overdraft-limit])
                        within-limit?))
(s/def ::balance integer?)
(s/def ::bonds (s/and set?
                      (s/every ::bond-ref)))
(s/def ::bond-ref bond-ref?)
(s/def ::bond (s/keys :req [::who ::amount]))

(def accounts)

(deftx make-bank []
  ;; FIXME: alter-var-root in tx?
  (alter-var-root #'accounts (fn [_] (pref {}))))

(defn get-account [name]
  (get @accounts name))

(deftx make-account! [name]
  {:pre [(not (get-account name))]}
  (palter accounts assoc name
          (pref {::name name
                 ::balance 0
                 ::bonds #{}}
                :validator (partial s/valid? ::account)))
  (get-account name))

(deftx set-overdraft-limit! [account limit]
  {:pre [(not (neg? limit))]}
  (palter account assoc ::overdraft-limit limit))

(deftx deposit! [account amount]
  {:pre [(pos? amount)]}
  (palter account update  ::balance + amount))

(deftx withdraw! [account amount]
  {:pre [(pos? amount)]}
  (palter account update ::balance - amount))

(deftx transfer! [from to amount]
  {:pre [(pos? amount)]}
  (withdraw! from amount)
  (deposit! to amount))

(deftx borrow! [from to amount]
  {:pre [(pos? amount)]}
  (palter from update ::bonds conj (pref {::who to ::amount amount}
                                         :validator (partial s/valid? ::bond)))
  (withdraw! from amount)
  (deposit! to amount))

(defn matching-bond [account who amount]
  (or (first (filter #(and (= (::who @%) who)
                           (= (::amount @%) amount))
                     (::bonds @account)))
      (throw+ {:type ::no-matching-bond
               ::acount account
               ::who who
               ::amount amount})))

(deftx repay! [from to amount]
  {:pre [(pos? amount)]}
  (palter to update-in [::bonds] disj (matching-bond to from amount))
  (withdraw! from amount)
  (deposit! to amount))
