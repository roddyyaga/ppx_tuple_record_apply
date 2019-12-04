# ppx_rapper
A ppx to derive helper functions for Rapper.

```ocaml
type foo = { a : int; b : string; c : bool } [@@deriving rapper]
```
produces
```ocaml
type tuple = int * string * bool
type 'a labelled_function = a:int -> b:string -> c:bool -> 'a
let foo_to_tuple {a; b; c} = (a, b, c)
let foo_of_tuple (a, b, c) = {a; b; c}
let foo_record_apply f {a; b; c} = f ~a ~b ~c
```.

Like ppx_deriving_yojson, if the type name is `t` then the generated functions will simply be `to_tuple`, `of_tuple` and
`record_apply`.
