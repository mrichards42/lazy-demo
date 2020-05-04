(ns lazy-demo.seq
  "Implementing clojure ISeq"
  (:require [lazy-demo.protocol :as p]))

(deftype DelaySnoozeSeq [d]
  ;; Implement snooze
  p/Snooze
  (wake [this]
    (force d))

  ;; Implement ISeq
  clojure.lang.ISeq
  (first [this] (p/zfirst this))
  (next [this] (p/zrest this))
  (more [this] (or (p/zrest this) ()))
  (cons [this x]
    (DelaySnoozeSeq. (delay {:first x :rest this})))

  ;; Seems like we also need to implement Sequable?
  clojure.lang.Seqable
  (seq [this] this))


(defmacro make-snooze [& body]
  `(DelaySnoozeSeq. (delay ~@body)))

(defn zrange [start end]
  (when (< start end)
    (make-snooze
     {:first start
      :rest (zrange (inc start) end)})))

(comment
  ;; Now that we participate in the ISeq interface, we can use clojure.core
  ;; sequence functions
  (->> (zrange 0 10)
       (map inc)
       (reduce +))

  (drop 2 (zrange 0 10))
  (take 10 (zrange 0 100000))

  (def x (zrange 0 10))
  (p/snooze? x)
  (seqable? x)
  (seq? x)
  (seq x)
  (vec x)

  (apply map * (repeat 3 (zrange 0 10)))
  )
