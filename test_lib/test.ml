open Core.Std

let%bench "test" [@indexed x = [100; 1000]] =
for _i = 1 to x do
  ignore (Array.init ~f:(fun i -> i) x);
  ignore (x*1)
done

let%bench "string" =
  ignore ("hello" ^ "world")
