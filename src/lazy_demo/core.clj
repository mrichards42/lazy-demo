(ns lazy-demo.core
  "lazy sequences from scratch.")

;; Laziness works by wrapping a value in a function to delay evaluation (you
;; might hear this called a "thunk", e.g.: https://en.wikipedia.org/wiki/Thunk)

(comment
  (def lazy
    (fn []
      ;; we're inside a function, so this is not evaluated yet
      (println "realized")
      1))

  (fn? lazy)
  ; => true

  ;; now we realize the value
  (lazy)
  ; "realized"
  ; => 1
  )










;; Let's call our lazy function "snooze", and define a couple of very simple
;; operations on snoozes.

(defmacro make-snooze
  "Defines a snooze (literally just another way to spell `fn` in this example)"
  [& body]
  `(fn [] ~@body))

(defn snooze?
  "Is this a snooze? (aka is this a function?)"
  [x]
  (fn? x))

(defn wake
  "'Wake up' a snooze (aka call the function)"
  [snooze]
  (snooze))

(defn fully-wake
  "Wakes up a snooze. If a snooze returns another snooze, wakes it up too."
  [snooze]
  (if (snooze? snooze)
    (recur (wake snooze))
    snooze))

(comment
  (def snoozed (make-snooze 1))         ; a snooze
  (def snoozed' (make-snooze snoozed))  ; a snooze wrapping a snooze

  (fully-wake snoozed)                  ; wake up a snooze
  ; => 1
  (fully-wake snoozed')                 ; wake up a double snooze
  ; => 1
  (fully-wake 1)                        ; passes values through unchanged
  ; => 1
  )










;; To make a lazy sequence, we need a data structure that includes a value (the
;; head of the sequence), and a function (which should return the tail)

(comment
  (def a-snooze
    (fn []
      {:first "first value"
       :rest (fn []
               ;; the "tail" returned by this function is the same data structure:
               ;; the next head and the next tail
               {:first "second value"
                :rest (fn []
                        {:first "third value"
                         :rest (fn [])})})}))

  ;; this could also be a cons cell, but we'll use :first and :rest in this
  ;; example for clarity
  ["first value" (fn [] ["second value" (fn [] ["third value" (fn [])])])]
  )










;; The most important functions operating on snoozes are functions to get the
;; head and tail (we'll prefix all our functions with `z` as in zzz)

(defn zfirst
  "Gets the first value from a snooze."
  [snooze]
  (:first (fully-wake snooze)))

(defn zrest
  "Gets the rest of a snooze seq."
  [snooze]
  (:rest (fully-wake snooze)))

(comment
  ;; Using the snooze we defined above
  (zfirst a-snooze)
  ; => "first value"

  (zrest a-snooze)
  ; => #function[lazy-demo.core/a-snooze/fn--11285]    (the :rest function)
  (snooze? (zrest a-snooze))
  ; => true

  (zfirst (zrest a-snooze))
  ; => "second value"

  (zfirst (zrest (zrest a-snooze)))
  ; => "third value"
  )










;; This is enough to make simple lazy range function. We'll start using
;; `make-snooze` instead of `fn`. `make-snooze` is our version of clojure's
;; `lazy-seq` macro

(defn zrange [start end]
  (when (< start end)
    ;; this appears to be recursive since we call `zrange` inside `zrange`, but
    ;; since `make-snooze` delays evaluation, the recursive call to `zrange`
    ;; isn't evaluated until after we've returned.
    (make-snooze
     {:first start
      :rest (zrange (inc start) end)})))

(comment
  (zfirst (zrange 0 10))           ; the first value in the range
  ; => 0
  (zfirst (zrest (zrange 0 10)))   ; the second value in the range
  ; => 1
  (zfirst (zrange 0 10000))        ; lazy, so no stack overflow
  ; => 0
  )









;; compare the above to this recursive-range, which uses _eager_ evaluation
(defn recursive-range [start end]
  (when (< start end)
    {:first start
     :rest (recursive-range (inc start) end)}))

(comment
  (zfirst (recursive-range 0 10))           ; ok
  ; => 0
  (zfirst (zrest (recursive-range 0 10)))   ; ok
  ; => 1
  (zfirst (recursive-range 0 10000))        ; oops, too eager
  ; StackOverflowError
  )










;;; Detour into cons

(comment
  ;; cons is used to `cons`truct a sequence by adding a item to the front.

  (cons 1 (list 2 3 4 5 6))
  ; => (1 2 3 4 5 6)

  ;; construct a sequence from scratch
  (cons 1 ())
  ; => (1)

  (cons 1 (cons 2 ()))
  ; => (1 2)

  (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 ()))))))
  ; => (1 2 3 4 5 6)

  (cons 0 (range 1 10))
  (cons 0 (cons 1 (range 2 10)))
  ; => (1 2 3 4 5 6 7 8 9)

  ;; Note: in most lisps cons is used to construct a concrete _list_ data
  ;; structure, whereas in clojure it is used to construct a _sequence_.
  ;; Clojure further confuses the matter by printing sequences with ().
  ;; But if you check the type:

  (type (cons 0 ()))
  ; => clojure.lang.Cons
  (type (list 1 2 3))
  ; => clojure.lang.PersistentList
  )










;; here's a version of cons that we can use to add a lazy value to a snooze

(defn zcons
  "Adds a value to the front of a snooze."
  [v snooze]
  {:first v
   :rest snooze})

;; And a helper to make reading things easier

(defn zdoall
  "Forces all items in a snooze."
  [snooze]
  (loop [result []
         snooze snooze]
    (if snooze
      (recur (conj result (zfirst snooze)) (zrest snooze))
      (seq result))))

(comment
  (zfirst (zcons 1 nil))
  ; => 1

  (zfirst (zrest (zcons 1 (zcons 2 nil))))
  ; => 2

  (zdoall (zrange 0 10))
  ; => (0 1 2 3 4 5 6 7 8 9)
  )










;; Now we can write zrange in terms of cons:

(comment
  (zdoall (zcons 0 nil))
  ; => (0)
  (zdoall (zcons 0 (zcons 1 nil)))
  ; => (0 1)
  (zdoall (zcons 0 (zcons 1 (zcons 2 nil))))
  ; => (0 1 2)
  )

(defn zrange' [start end]
  (when (< start end)
    (make-snooze
     (zcons start (zrange' (inc start) end)))))

;; Note that you could write this exact same function using lazy-seq and cons,
;; which returns a lazy sequence you can use with normal clojure functions
(defn lazy-range [start end]
  (lazy-seq
   (when (< start end)
     (cons start (lazy-range (inc start) end)))))

(comment
  (zdoall (zrange' 0 10))
  ; => (0 1 2 3 4 5 6 7 8 9)

  (doall (lazy-range 0 10))
  ; => (0 1 2 3 4 5 6 7 8 9)
  )










;; `cons` and `lazy-seq` are the foundation of all lazy clojure functions;
;; `zcons` and `make-snooze` are the foundation of all our demo lazy functions:

(defn ztake
  "Takes the first `n` items from a snooze."
  [n snooze]
  (when (and snooze (pos? n))
    ;; delayed evaluation
    (make-snooze
     ;; returning a sequence of
     (zcons ;; the head of the current snooze
            (zfirst snooze)
            ;; and (lazily) `take n-1` from the rest of the snooze
            (ztake (dec n) (zrest snooze))))))

(comment
  (->> (zrange 0 100)
       (ztake 10)
       (zdoall))
  ; => (0 1 2 3 4 5 6 7 8 9)

  (->> (zrange 0 1000000)
       (ztake 10)
       (zdoall))

  (->> (range 0 1000000)
       (take 10)
       (doall))

  (->> (range 0 100)
       (take 10)
       (doall))
  ; => (0 1 2 3 4 5 6 7 8 9)

  ;; compare to the clojure source
  (clojure.repl/source take)
  )











(defn zmap
  "Maps `f` over all items in `snooze`"
  [f snooze]
  (when snooze
    ;; delayed evaluation
    (make-snooze
     ;; returning a sequence of
     (zcons ;; `f` called with the head of the snooze
            (f (zfirst snooze))
            ;; and (lazily) `map f` over the rest of the snooze
            (zmap f (zrest snooze))))))

(comment
  (->> (zrange 0 10)
       (zmap #(* 10 %))
       (zdoall))
  ; => (0 10 20 30 40 50 60 70 80 90)

  (->> (range 0 10)
       (map #(* 10 %))
       (doall))
  ; => (0 10 20 30 40 50 60 70 80 90)

  ;; compare to clojure source
  (clojure.repl/source map)
  )










(defn zfilter
  "Filters items in `snooze` using predicate `f`."
  [pred snooze]
  (when snooze
    (make-snooze
     (if (pred (zfirst snooze))
       (zcons (zfirst snooze)
              (zfilter pred (zrest snooze)))
       (zfilter pred (zrest snooze))))))

(comment
  (->> (zrange 0 10)
       (zfilter odd?)
       (zdoall))
  ; => (1 3 5 7 9)

  ;; compare to clojure source
  (clojure.repl/source filter)
  )










;;; A little more fun

(defn zrepeat [n x]
  (when (pos? n)
    ;; delayed evaluation
    (make-snooze
     ;; returning a sequence of
     (zcons ;; x
            x
            ;; and (lazily) repeat n-1 more times
            (zrepeat (dec n) x)))))

(defn ziterate [f x]
  (make-snooze
   (zcons x (ziterate f (f x)))))

(defn zconcat [z1 z2]
  ;; delayed evaluation
  (make-snooze
   ;; returning either
   (if z1
     ;; a sequence of
     (zcons ;; the first item in z1
            (zfirst z1)
            ;; and (lazily) the rest of z1 + all of z2
            (zconcat (zrest z1) z2))
     ;; or if we've exhausted z1, all of z2
     z2)))

(defn zcycle [snooze]
  ;; (lazily) concat
  (make-snooze
   (zconcat ;; the original sequence
            snooze
            ;; plus an infinite cycle of the original sequence
            (zcycle snooze))))

(comment
  ;; Most of these are not interesting since they're optimized in Java
  (clojure.repl/source repeat)
  (clojure.repl/source iterate)
  (clojure.repl/source concat)
  (clojure.repl/source cycle)
  )

(comment
  (->> (ziterate inc 0)
       (ztake 10)
       (zdoall))

  (->> (ziterate (partial * 2) 1)
       (ztake 20)
       (zdoall))

  ;; Chris's parrotwave
  (->> (ziterate inc 1)
       (zmap #(str ":parrotwave" % ":"))
       (ztake 9)
       (zdoall))

  ;; Mike's parrotwave
  (->> (zrange 1 10)
       (zcycle)
       (zmap (partial format ":parrotwave%d:"))
       (ztake 100)
       (zdoall))
  )












;; And some more fun -- effectively we've implemented lazy cons cells

(def car zfirst)
(def cdr zrest)
(def cadr (comp car cdr))           ; second
(def caddr (comp car cdr cdr))      ; third
(def cadddr (comp car cdr cdr cdr)) ; fourth

(comment
  (car (zrange 0 10))
  (cadr (zrange 0 10))
  (caddr (zrange 0 10))
  (cadddr (zrange 0 10))
  )
