type backend = 
    | LocalHashTbl
    | GlobalHashTbl 
    | IrminFs: Irmin.config -> backend
    | IrminFsOnly: Irmin.config -> backend
    | IrminMem

val memoize: backend -> ('a -> 'b) -> ('a -> 'b)
val memoize2: backend -> ('a -> 'b -> 'c) -> ('a -> 'b -> 'c)
val memoize3: backend -> ('a -> 'b -> 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
val memoize4: backend -> ('a -> 'b -> 'c -> 'd -> 'e) -> ('a -> 'b -> 'c -> 'd -> 'e)