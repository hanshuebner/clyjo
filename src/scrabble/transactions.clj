(ns scrabble.transactions
  (:import [clojure.lang IDeref])
  (:require [clojure.spec :as s]
            [clojure.pprint :as pprint]))

;;; Adapted from http://blog.klipse.tech/clojure/2016/10/10/defn-args-2.html

;; Plumbing for deftx macro so that it can use a full defn spec with
;; argument list restrictions.

(defn update-conf [{[arity] :bs :as conf} body-update-fn]
  (case arity
    :arity-1 (update-in conf [:bs 1 :body]
                        body-update-fn)
    :arity-n (update-in conf [:bs 1 :bodies]
                        (partial map #(update % :body body-update-fn)))))

(defn extract-signature [{:keys [name]
                          [arity {:keys [args]}] :bs
                          :as conf}]
  (assert (not (:varargs args))
          "transaction functions can't have variable argument lists")
  (assert (= arity :arity-1)
          "transaction functions can only have one arity")
  (assert (apply = :sym (map first (:args args)))
          "transaction functions must have simple symbols as arguments")
  `('~name ~@(map second (:args args))))

;; Persistent refs are references that represent a persistent entity.
;; They consist of the persistent ID of the entity and a ref to the
;; object.

(def id-counter (atom 0))

(defrecord PersistentRef [id ref]
  IDeref
  (deref [_] (deref ref)))

(defmethod print-method PersistentRef [value ^java.io.Writer writer]
  (.write writer "#PersistentRef")
  (.write writer (pr-str {:id (:id value)})))

;; Found in https://gist.github.com/rwilson/34c88a97c6260a7dc703
(defmethod pprint/simple-dispatch PersistentRef [o]
  ((get-method clojure.pprint/simple-dispatch clojure.lang.IPersistentMap) o))

(defn pref [value & args]
  (PersistentRef. (swap! id-counter inc) (apply ref value args)))

(defn pref-set [ref value]
  (ref-set (:ref ref) value))

(defn log-transaction [name & args]
  (println "log-transaction" name args))

(def ^:dynamic *within-transaction* false)

(defn wrap-transaction [signature body]
  `((let [return-value# (binding [*within-transaction* true]
                          (dosync ~@body))]
      (when-not *within-transaction*
        (log-transaction ~@signature))
      return-value#)))

(defmacro deftx [& args]
  (let [{:keys [name] :as conf} (s/conform :scrabble.defn-specs/defn-args args)]
    (->> (extract-signature conf)
         (partial wrap-transaction)
         (update-conf conf)
         (s/unform :scrabble.defn-specs/defn-args)
         (cons `defn))))
