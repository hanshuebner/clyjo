(ns scrabble.transactions
  (:import [clojure.lang IDeref])
  (:require [clojure.spec :as s]
            [clojure.pprint :as pprint]
            [slingshot.slingshot :refer [throw+]]
            [clojure.java.io :as io]))

;; FIXME:
;; transactions are logged in a separae thread, asynchronously.

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

(def ^:dynamic *logger* nil)

(defn make-logger [logfile]
  {:output-stream (io/writer (io/file logfile)
                             :append true)
   :id-counter (ref 0)
   :transaction-counter (ref 0)})

(defn open-logger [logfile]
  (alter-var-root #'*logger* (fn [old-logger]
                               (when old-logger
                                 (.close (:output-stream old-logger)))
                               (agent (make-logger logfile)))))

(defn close-logger []
  (alter-var-root #'*logger* (fn [logger]
                               (when logger
                                 (.close (:output-stream @logger)))
                               nil)))

;; Persistent refs are references that represent a persistent entity.
;; They consist of the persistent ID of the entity and a ref to the
;; object.

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
  (assert *logger* "Logger not opened")
  (PersistentRef. (alter (:id-counter @*logger*) inc)
                  (apply ref value args)))

(defn pref-set [ref value]
  (ref-set (:ref ref) value))

(defn palter [ref f & args]
  (apply alter (:ref ref) f args))

(defn log-transaction [{:keys [output-stream last-transaction-counter] :as logger}
                       transaction-counter name & args]
  (when (and last-transaction-counter
             (<= transaction-counter last-transaction-counter))
    (throw+ {:type ::inconsistent-transaction-ordering
             :transaction-counter transaction-counter
             :last-transaction-counter last-transaction-counter}))
  (.write output-stream
          (binding [*print-length* nil
                    *print-level* nil
                    *print-readably* true]
            (prn-str (cons name args))))
  (.flush output-stream)
  (assoc logger :last-transaction-counter transaction-counter))

(def ^:dynamic *within-transaction* false)

(defn wrap-transaction [signature body]
  `((assert *logger* "Logger not opened")
    (dosync
     (let [return-value# (binding [*within-transaction* true]
                           ~@body)]
       (when-not *within-transaction*
         (send *logger* log-transaction (alter (:transaction-counter @*logger*) inc) ~@signature))
       return-value#))))

(defmacro deftx [& args]
  (let [{:keys [name] :as conf} (s/conform :scrabble.defn-specs/defn-args args)]
    (->> (extract-signature conf)
         (partial wrap-transaction)
         (update-conf conf)
         (s/unform :scrabble.defn-specs/defn-args)
         (cons `defn))))
