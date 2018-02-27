open RememberMe
open OUnit2

let config = Irmin_git.config ~root:"db" ()

let add1 x = x+1
let test_add1_local test_ctx = assert_equal 3 @@ (memoize LocalHashTbl add1) 2
let test_add1_global test_ctx = assert_equal 3 @@ (memoize GlobalHashTbl add1) 2
let test_add1_mem test_ctx = assert_equal 3 @@ (memoize IrminMem add1) 2
let test_add1_fs test_ctx = assert_equal 3 @@ (memoize (IrminFs config) add1) 2
let suite = 
    "suite" >:::
    [
        "test_add1_local">::test_add1_local;
        "test_add1_global">::test_add1_global;
        "test_add1_mem">::test_add1_mem;
        "test_add1_fs">::test_add1_fs
    ]

let () =
    run_test_tt_main suite
