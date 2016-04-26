#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"core_bench"
  [ oasis_lib "core_bench"
  ; oasis_lib "inline_benchmarks"
  ; file "META" ~section:"lib"
  ]
