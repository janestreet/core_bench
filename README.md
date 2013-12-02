Core_bench
==========

Core_bench is a micro-benchmarking library for OCaml that can
measure execution costs of operations that take 1ns to about
100ms. Core_bench tries to measure execution costs of such
short-lived computations precisely while trying to account for
delayed GC costs and noise introduced by other activity on the
system.

Micro-benchmarking enables programmers make informed choices when
writing performance sensitive code. In the words of Michael
Abrash from the old "Zen and the Art of Code Optimization":

> Assume nothing. I cannot emphasize this strongly enough-when you
> care about performance, do your best to improve the code and
> then measure the improvement. If you don't measure performance,
> you're just guessing, and if you're guessing, you're not very
> likely to write top-notch code

Core_bench shares some similarities with Haskell's Criterion,
though there are significant differences in the underlying
approach.

Install using:
<pre>
opam install core_bench
</pre>

To get started see:
https://github.com/janestreet/core_bench/wiki/Getting-Started-with-Core_bench


