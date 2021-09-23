module Bench = Bench

module Core_bench_private = struct
  module Analysis_result = Analysis_result (* used in expect tests *)

  module Linear_algebra = Linear_algebra (* used in expect tests *)

  module Simplified_benchmark = Simplified_benchmark (* needed by core_bench/comparison *)
end
