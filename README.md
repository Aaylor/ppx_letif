ppx letif
=========

*Experimental repository*

This ppx script allows to write the same structure "if let" made in Swift.

The first example with only one pattern:
```
let value = Some 42
let () = let%if Some v = value in print_int v

(* is translated into *)
let value = Some 42
let () = match value with Some v -> print_int v | _ -> ()
```

Without any else clause, it assumes that the type is unit.

An example with mutliple patterns:
```
let value = Some 20
let l = [22]
let () = let%if Some a = value and [b] = l in print_int (a + b)

(* is translated into *)
let value = Some 20
let l = [22]
let () = match value, l with (Some a, b :: []) -> print_int (a + b) | _ -> ()
```

Example with else clause:
```
let value = Some 20
let res = (let%if Some a = value in a)[%else 42]
  
(* is translated into *)
let value = Some 20
let res = match value with | Some a -> a | _ -> 42
```


