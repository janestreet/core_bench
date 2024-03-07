"Core_bench"
============


`Core_bench` is a library for running benchmarks written
using the `ppx_bench` syntax extension.

See https://blog.janestreet.com/core_bench-micro-benchmarking-for-ocaml/ for the 
introduction.

To run the benchmarks when using `dune`, you usually need to define
your own executable that calls `Inline_benchmarks_public.Runner.main`. 
You need to make sure that the benchmarks are linked into this executable, usually 
by adding `(link_flags -linkall)` to the executable stanza to make sure the
library dependencies are not dropped by the compiler.
