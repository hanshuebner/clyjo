(ns scrabble.transactions
  (:import [clojure.lang IDeref])
  (:require [clojure.spec :as s]
            [clojure.pprint :as pprint]
            [slingshot.slingshot :refer [throw+]]
            [clojure.java.io :as io]))

(def ^:dynamic *store* nil)

(def ^:dynamic *log-transaction* true)

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

;; Persistent refs are references that represent a persistent entity.
;; They consist of the persistent ID of the entity and a ref to the
;; object.

(defrecord PersistentRef [id ref]
  IDeref
  (deref [_] (deref ref)))

(defmethod print-method PersistentRef [value ^java.io.Writer writer]
  (.write writer "#PersistentRef")
  (.write writer (pr-str {:id (:id value)})))

(defmethod print-dup PersistentRef [value ^java.io.Writer writer]
  (.write writer (format "(scrabble.transactions/lookup %d)" (:id value))))

;; Found in https://gist.github.com/rwilson/34c88a97c6260a7dc703
(defmethod pprint/simple-dispatch PersistentRef [o]
  ((get-method clojure.pprint/simple-dispatch clojure.lang.IPersistentMap) o))

(defn pref [value & args]
  (assert *store* "Store not opened")
  (let [id (alter (:id-counter @*store*) inc)
        ref (PersistentRef. id (apply ref value args))]
    (alter (:object-table @*store*) assoc id ref)
    ref))

(defn pref-set [ref value]
  (ref-set (:ref ref) value))

(defn palter [ref f & args]
  (apply alter (:ref ref) f args))

(defn lookup [id]
  (assert *store* "Store not opened")
  (or (get @(:object-table @*store*) id)
      (throw+ {:type ::invalid-entity-id
               :id id})))

(defn restore [logfile]
  (dosync
   (with-open [in (java.io.PushbackReader. (io/reader logfile))]
     (binding [*read-eval* true
               *log-transaction* false]
       (loop []
         (when-let [form (read in false false)]
           (eval form)
           (recur)))))))

(defn make-store [logfile]
  {:output-stream (io/writer (io/file logfile)
                             :append true)
   :id-counter (ref 0)
   :object-table (ref {})
   :transaction-counter (ref 0)})

(defn open-store [logfile]
  (alter-var-root #'*store* (fn [old-store]
                              (when old-store
                                (.close (:output-stream @old-store)))
                              (agent (make-store logfile))))
  (when (.exists (io/file logfile))
    (restore logfile)))

(defn close-store []
  (alter-var-root #'*store* (fn [store]
                              (when store
                                (.close (:output-stream @store)))
                              nil)))

(defn log-transaction [{:keys [output-stream last-transaction-counter] :as store}
                       transaction-counter logged?
                       function-name & args]
  (when (and last-transaction-counter
             (<= transaction-counter last-transaction-counter))
    (throw+ {:type ::inconsistent-transaction-ordering
             :transaction-counter transaction-counter
             :last-transaction-counter last-transaction-counter}))
  (.write output-stream
          (binding [*print-length* nil
                    *print-level* nil
                    *print-readably* true
                    *print-dup* true]
            (prn-str (cons function-name args))))
  (.flush output-stream)
  (deliver logged? true)
  (assoc store :last-transaction-counter transaction-counter))

(defmacro returning
  "Compute a return value, then execute other forms for side effects.
  Like prog1 in common lisp, or a (do) that returns the first form."
  [value & forms]
  `(let [value# ~value]
     ~@forms
     value#))

(defn wrap-transaction [[[_ name] & signature] body]
  `((assert *store* "Store not opened")
    (let [logged?# (promise)]
      (returning (dosync
                  (returning (binding [*log-transaction* false]
                               ~@body)
                             (when *log-transaction*
                               (send *store*
                                     log-transaction
                                     (alter (:transaction-counter @*store*) inc)
                                     logged?#
                                     ~(resolve name)
                                     ~@signature))))
                 (when *log-transaction*
                   (deref logged?#))))))

(defmacro deftx [& args]
  (let [conf (s/conform :scrabble.defn-specs/defn-args args)]
    (->> (extract-signature conf)
         (partial wrap-transaction)
         (update-conf conf)
         (s/unform :scrabble.defn-specs/defn-args)
         (cons `defn))))
