module Record = struct
  type t = { a : int; b : string; c : bool } [@@deriving rapper]
end

let x = Record.{ a = 42; b = "foo"; c = false }

let x_a, x_b, x_c = Record.(to_tuple x)

let () =
  assert (x.a = x_a);
  assert (x.b = x_b);
  assert (x.c = x_c)

let y_a, y_b, y_c = (12, "bar", true)

let y = Record.(of_tuple (y_a, y_b, y_c))

let () =
  assert (y.a = y_a);
  assert (y.b = y_b);
  assert (y.c = y_c)

let f ~a ~b ~c = a + String.length b + if c then 90 else 0

let () = assert (Record.(record_apply f x = 45))
