(ns scrabble.core
  (:require [clojure.spec :as s]
            [scrabble.defn-specs]
            [scrabble.transactions :refer [deftx pref-set palter pref]]
            [slingshot.slingshot :refer [throw+]]))

(def accounts)

(deftx make-bank []
  ;; FIXME: alter-var-root in tx?
  (alter-var-root #'accounts (fn [_] (pref {}))))

(defn get-account [name]
  (get @accounts name))

(defn within-limit? [{:keys [::balance ::overdraft-limit]
                      :or {::overdraft-limit 0}}]
  (>= balance (- overdraft-limit)))

(s/def ::balance integer?)
(s/def ::bond (s/keys :req [::who
                            ::amount]))
(s/def ::bonds (s/* ::bond))
(s/def ::account (s/and (s/keys :req [::balance
                                      ::name]
                                :optional [::overdraft-limit
                                           ::bonds])
                        within-limit?))

(deftx make-account! [name]
  {:pre [(not (get-account name))]}
  (->> (pref {::name name
              ::balance 0}
             :validator (partial s/valid? ::account))
       (assoc @accounts name)
       (pref-set accounts))
  (get-account name))

(deftx set-overdraft-limit! [account limit]
  {:pre [(s/valid? ::account @account)
         (not (neg? limit))]}
  (->> (assoc @account ::overdraft-limit limit)
       (pref-set account)))

(deftx deposit! [account amount]
  {:pre [(s/valid? ::account @account)
         (pos? amount)]}
  (->> (update @account ::balance + amount)
       (pref-set account)))

(deftx withdraw! [account amount]
  {:pre [(s/valid? ::account @account)
         (pos? amount)]}
  (->> (update @account ::balance - amount)
       (pref-set account)))

(deftx transfer! [from to amount]
  {:pre [(and (s/valid? ::account @from)
              (s/valid? ::account @to)
              (pos? amount))]}
  (withdraw! from amount)
  (deposit! to amount))

(deftx borrow! [from to amount]
  {:pre [(and (s/valid? ::account @from)
              (s/valid? ::account @to)
              (pos? amount))]}
  (palter from update ::bonds conj {::who to ::amount amount})
  (withdraw! from amount)
  (deposit! to amount))

(deftx repay! [from to amount]
  {:pre [(and (s/valid? ::account @from)
              (s/valid? ::account @to)
              (pos? amount))]}
  (let [bonds (set (::bonds to))
        bond (first (filter #(and (= (::who %) from)
                                  (= (::amount %) amount))
                            bonds))]
    (throw+ {:type ::no-matching-bond
             ::from from
             ::amount amount})
    (palter to assoc ::bonds (dissoc bonds bond))
    (withdraw! from amount)
    (deposit! to amount)))
