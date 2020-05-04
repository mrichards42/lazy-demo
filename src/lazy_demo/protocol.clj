(ns lazy-demo.protocol
  "A protocol-based snooze, with a delay impl.")

;; Ignore the boilerplate and skip to line 146 for the interesting stuff

(comment
  ;; A `delay` prevents evaluation of its body until the first time it is
  ;; derefenced. The value is only evaluated the first time.
  (def x
    (delay
     (println "hi!")
     (+ 1 1)))

  (prn x)
  ; => #delay[{:status :pending, :val nil} 0x160ab28]

  (prn (force x))
  ; "hi"
  ; => 2

  (prn x)
  ; => #delay[{:status :ready, :val 2} 0x160ab28]

  (prn (force x))
  ; => 2
  )










;;; Snooze protocol

(defprotocol Snooze
  (wake [this]))

(defn snooze? [x]
  (satisfies? Snooze x))

(defn fully-wake
  "Wakes up a snooze. If a snooze returns another snooze, wakes it up too."
  [snooze]
  (if (snooze? snooze)
      (recur (wake snooze))
      snooze))

;;; DelaySnooze impl

;; Clojure lazy sequences are implemented in a similar manner as delays. Each
;; value is only calculated once. This is an implementation of `snooze` using
;; delays instead of just functions:

(defrecord DelaySnooze [d]
  Snooze
  (wake [_] (force d)))

(defmacro make-snooze [& body]
  `(->DelaySnooze (delay ~@body)))








;; The following function implementations are exactly the same as the original
;; ones in `core`. The `Snooze` protocol means that anything that implements
;; the single `wake` function can act like a `snooze`.

;;; basic snooze operations

(defn zfirst
  "Gets the first value from a snooze."
  [snooze]
  (:first (fully-wake snooze)))

(defn zrest
  "Gets the rest of a snooze seq."
  [snooze]
  (:rest (fully-wake snooze)))

;; More snooze-compatibile functions

(defn zcons
  "Adds a value to the front of a snooze."
  [v snooze]
  (make-snooze
   {:first v
    :rest snooze}))

(defn zdoall
  "Forces all items in a snooze."
  [snooze]
  (loop [result []
         snooze snooze]
    (if snooze
      (recur (conj result (zfirst snooze)) (zrest snooze))
      (seq result))))

(defn ztake
  "Takes the first `n` items from a snooze."
  [n snooze]
  (when (and snooze (pos? n))
    (make-snooze
     (zcons (zfirst snooze)
            (ztake (dec n) (zrest snooze))))))

(defn zmap
  "Maps `f` over all items in `snooze`"
  [f snooze]
  (when snooze
    (make-snooze
     (zcons (f (zfirst snooze))
            (zmap f (zrest snooze))))))

(defn zfilter
  "Filters items in `snooze` using predicate `f`."
  [pred snooze]
  (when snooze
    (make-snooze
     (if (pred (zfirst snooze))
       (zcons (zfirst snooze)
              (zfilter pred (zrest snooze)))
       (zfilter pred (zrest snooze))))))

(defn zrepeat [n x]
  (when (pos? n)
    (make-snooze
     (zcons x (zrepeat (dec n) x)))))










;;; Lazy side-effects (bad news)

(defn lazy-range [start end]
  (when (< start end)
    (lazy-seq
     (cons start (lazy-range (inc start) end)))))

(defn zrange [start end]
  (when (< start end)
    (make-snooze
     (zcons start (zrange (inc start) end)))))

(comment
  ;; Nothing is printed at first, since nothing is realized
  (def printing-zrange
    (->> (zrange 0 1000)
         (zfilter even?)
         (zmap println)))

  (def printing-range
    (->> (range 0 1000)
         (filter even?)
         (map println)))

  ;; We have to call `doall` to realize the sequence
  (->> printing-zrange
       (ztake 5)
       (zdoall))

  ;; Many clojure seqs are chunked by default, such as `range`
  (->> printing-range
       (take 5) ; even though we're taking 5, we printed up to 32
       (doall))

  ;; Not chunked
  (def printing-lazy-range
    (->> (lazy-range 0 1000)
         (filter even?)
         (map println)))

  (->> printing-lazy-range
       (take 5) ; take 5 and only print 5
       (seq))


  ;; Note: some functions understand chunking (e.g. map, filter), but not all
  ;; of them (e.g. take). So these two are actually not quite equivalent
  (->> (range 0 1000)
       (take 5)
       (map println)
       (doall))

  (->> (range 0 1000)
       (map println)
       (take 5)
       (doall))
  )








;;; Laziness and expensive calculations

(defn expensive [n]
  (Thread/sleep n)
  "done")

(comment
  ;; A lazy sequence of expensive calculations. No calculation is done yet.
  (def expenseive-seq
    (->> (zrepeat 100 1000)
         (zmap expensive)))

  ;; Realize the first 2 items. The first time you call this it should take 2
  ;; seconds. Subsequent calls will be instant since the result is cached.
  (time
   (->> expenseive-seq
        (ztake 2)
        (zdoall)))

  ;; Realize 1 more item, which should take 1 more second (just the first time)
  (time
   (->> expenseive-seq
        (ztake 3)
        (zdoall)))


  ;; chunking + lazy expensive operations = more waiting than you expected
  ;; Note: repeat isn't chunked, but vectors are
  (def expensive-clojure-seq
    (->> (vec (repeat 100 1000))
         (map expensive)))

  ;; taking the first 2 from an expensive _chunked_ sequence actually results
  ;; in the first 32 items being evaluated
  (time
   (->> expensive-clojure-seq
        (take 2)
        (doall)))
  )
