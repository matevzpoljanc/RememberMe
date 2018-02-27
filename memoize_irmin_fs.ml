let counter = ref 0
let new_function_id () = 
  let count= !counter in 
      counter := count + 1; count

module IrminHashTbl = Memo_result.MemoResult(Irmin_fs.AO(Git_unix.FS.IO))(Irmin_mem.RW)

let (>>=) = Lwt.bind

let m = Lwt_main.run (IrminHashTbl.create @@ Irmin_fs.config ~root:"db" () )

let memoize (type a) (type b) m (f:a -> b): a -> b =
  let f_id = new_function_id () in
  let g x =
    Lwt_main.run @@ IrminHashTbl.find_or_add m (f_id,x) ~default:(fun () -> (f x)) in
  g
  ;;

let memoize2 (type a) (type b) (type c) m (f:a -> b -> c): a -> b -> c =
  let f_id = new_function_id () in
  let g x0 x1 =
    Lwt_main.run @@ IrminHashTbl.find_or_add m (f_id, x0, x1) ~default:(fun () -> (f x0 x1)) in
  g
  ;;

let memoize3 (type a) (type b) (type c) (type d) m (f:a -> b -> c -> d): a -> b -> c -> d =
  let f_id = new_function_id () in
  let g x0 x1 x2 =
    Lwt_main.run @@ IrminHashTbl.find_or_add m (f_id, x0, x1, x2) ~default:(fun () -> (f x0 x1 x2)) in
  g
  ;;

let memoize4 (type a) (type b) (type c) (type d) (type e) m (f:a -> b -> c -> d -> e): a -> b -> c -> d -> e =
  let f_id = new_function_id () in
  let g x0 x1 x2 x3 =
    Lwt_main.run @@ IrminHashTbl.find_or_add m (f_id, x0, x1, x2, x3) ~default:(fun () -> (f x0 x1 x2 x3)) in
  g
  ;;
let double = memoize m (fun x -> x*2);;
let int_of_bool b = if b then 1 else 0;;

let m_int = memoize m int_of_bool;;
let m_string_of_int = memoize m string_of_int;; 

assert ((memoize2 m (+) 2 3) = 5);
assert ((memoize2 m (+.) 2. 3.) = 5.);
assert (double 1 = 2);
assert (m_int true = 1);
assert (double 4 = 8);
assert (m_string_of_int 1 = "1")