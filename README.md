# lazy-demo

A lazy-sequence demo

## Namespaces

### lazy-demo.core

A from-scratch implementation of lazy sequences and the sequence abstraction.
This is very similar to how Clojure does it, with some notable differences,
like not caching items in lazy sequences. 

See also:

* [`clojure.lang.LazySeq`][LazySeq]
* `clojure.core` sequence functions, e.g. the relevant parts of
  [map][core-map], [filter][core-filter], and [take][core-take]

### lazy-demo.protocol

A protocol for our lazy-sequence abstraction, and a re-implementation using
`delay` so that function calls are cached.

Also some examples of laziness + [side-effects][effect-seq] (guest starring [chunked sequences][chunked-seq]), and
laziness + [expensive calculations][expensive-seq].

### lazy-demo.seq

Implementing the Clojure sequence interfaces, so we can use regular clojure
sequence functions instead of our own homegrown versions.


[LazySeq]: https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/LazySeq.java
[core-map]: https://github.com/clojure/clojure/blob/clojure-1.10.1/src/clj/clojure/core.clj#L2755
[core-filter]: https://github.com/clojure/clojure/blob/clojure-1.10.1/src/clj/clojure/core.clj#L2820-L2823
[core-take]: https://github.com/clojure/clojure/blob/clojure-1.10.1/src/clj/clojure/core.clj#L2882-L2886
[effect-seq]: src/lazy_demo/protocol.clj#L146
[chunked-seq]: src/lazy_demo/protocol.clj#L175
[expensive-seq]: src/lazy_demo/protocol.clj#L211
