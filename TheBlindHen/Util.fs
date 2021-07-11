module Util

let _rnd : System.Random option ref = ref None
let getRandom() = (!_rnd).Value

let _verbose = ref false
let verbose () = !_verbose

/// Works on all types 'a that can be hashed 
let memoize (f: 'a -> 'b) =
    let memo = System.Collections.Generic.Dictionary()
    fun x ->
        match memo.TryGetValue x with
        | true, y -> y
        | false, _ ->
            let y = f x
            memo.Add (x,y)
            y