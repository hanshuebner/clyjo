(ns scrabble.core
  (:require [clojure.spec :as s]
            [scrabble.defn-specs]
            [scrabble.transactions :refer [deftx pref-set pref]]))

(def accounts (pref {}))

(deftx clear-all-accounts! []
  (pref-set accounts {}))

(defn get-account [name]
  (get @accounts name))

(defn within-limit? [{:keys [::balance ::overdraft-limit]
                      :or {::overdraft-limit 0}}]
  (>= balance (- overdraft-limit)))

(s/def ::balance integer?)
(s/def ::account (s/and (s/keys :req [::balance ::name]
                                :optional [::overdraft-limit])
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
  {:pre [(s/conform ::account account)
         (not (neg? limit))]}
  (->> (assoc @account ::overdraft-limit limit)
       (pref-set account)))

(deftx deposit! [account amount]
  {:pre [(s/conform ::account account)
         (pos? amount)]}
  (->> (update @account ::balance + amount)
       (pref-set account)))

(deftx withdraw! [account amount]
  {:pre [(s/conform ::account account)
         (pos? amount)]}
  (->> (update @account ::balance - amount)
       (pref-set account)))

(deftx transfer! [from to amount]
  {:pre [(and (s/conform ::account from)
              (s/conform ::account to))]}
  (withdraw! from amount)
  (deposit! to amount))
