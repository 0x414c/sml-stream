(* An SML/NJ implementation of Streams aka Lazy or Infinite Lists. *)

(* Needs to be enabled. Requires SML/NJ ver. 110.50 or later. *)
Control.lazysml := true;

(*
signature STREAM =
  sig
    datatype 'a stream! = >> of 'a * 'a stream | ends
    type 'a stream = 'a stream! ?.susp
    exception EmptyStream
    exception IndexError
    val toList : 'a stream! ?.susp -> 'a list
    val first : 'a stream! ?.susp -> 'a
    val tail : 'a stream! ?.susp -> 'a stream! ?.susp
    val tail_ : 'a stream! ?.susp -> 'a stream!
    val getNth : int -> 'a stream -> 'a
    val countAll : 'a stream! ?.susp -> int
    val countIf : ('a -> bool) -> 'a stream! ?.susp -> int
    val count : ''a -> ''a stream! ?.susp -> int
    val findIf : ('a -> bool) -> 'a stream! ?.susp -> 'a option
    val some : ('a -> bool) -> 'a stream! ?.susp -> bool
    val every : ('a -> bool) -> 'a stream! ?.susp -> bool
    val none : ('a -> bool) -> 'a stream! ?.susp -> bool
    val contains : ''a -> ''a stream! ?.susp -> bool
    val apply : ('a -> 'b) -> 'a stream! ?.susp -> unit
    val transform : ('a -> 'b) -> 'a stream! ?.susp -> 'b stream! ?.susp
    val foldLeft : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
    val foldRight : ('a * 'b -> 'b) -> 'b -> 'a stream! ?.susp -> 'b
    val takeIf : ('a -> bool) -> 'a stream! ?.susp -> 'a stream! ?.susp
    val rejectIf : ('a -> bool) -> 'a stream! ?.susp -> 'a stream! ?.susp
    val reject : ''a -> ''a stream! ?.susp -> ''a stream! ?.susp
    val rejectFirstIf : ('a -> bool) -> 'a stream! ?.susp -> 'a stream! ?.susp
    val rejectFirst : ''a -> ''a stream! ?.susp -> ''a stream! ?.susp
    val takeN : int -> 'a stream -> 'a stream! ?.susp
    val takeN_ : int -> 'a stream -> 'a stream!
    val takeNToList : int -> 'a stream -> 'a list
    val takeWhile : ('a -> bool) -> 'a stream! ?.susp -> 'a stream! ?.susp
    val takeWhileToList : ('a -> bool) -> 'a stream -> 'a list
    val rejectN : int -> 'a stream -> 'a stream! ?.susp
    val rejectN_ : int -> 'a stream -> 'a stream!
    val rejectNToList : int -> 'a stream -> 'a list
    val rejectWhile : ('a -> bool) -> 'a stream! ?.susp -> 'a stream! ?.susp
    val rejectWhileToList : ('a -> bool) -> 'a stream -> 'a list
    val concatenate : 'a stream -> 'a stream! ?.susp -> 'a stream! ?.susp
    val concatenate_ : 'a stream -> 'a stream! ?.susp -> 'a stream!
    val zip : ('a * 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream! ?.susp
    val zipToTuple : 'a stream -> 'b stream -> ('a * 'b) stream! ?.susp
    val zipToTuple_ : 'a stream -> 'b stream -> ('a * 'b) stream!
    val interleave : 'a stream! ?.susp -> 'a stream -> 'a stream! ?.susp
    val interleave_ : 'a stream! ?.susp -> 'a stream -> 'a stream!
    val reverse : 'a stream -> 'a stream! ?.susp
    val splitIfNot : ('a -> bool) -> 'a stream -> 'a stream! ?.susp * 'a stream! ?.susp
    val splitIf : ('a -> bool) -> 'a stream -> 'a stream! ?.susp * 'a stream! ?.susp
    val partitionBy : ('a -> bool) -> 'a stream! ?.susp -> 'a stream! ?.susp * 'a stream
    val ones : int stream! ?.susp
    val naturals : int stream! ?.susp
  end;
*)

(* Stream structure. *)
structure Stream =
  struct
    (* `stream` -- [lazy] Stream datatype: *)
    (* `>>` acts like built-in cons operator `::`, `ends` acts like built-in `nil`. *)
    datatype lazy 'a stream = ends | >> of 'a * 'a stream

    (* Cons `>>` operator w/ same precedence as built-in cons operator `::`. *)
    infixr 5 >>

    (* `EmptyStream` -- Raised when output cannot be produced from empty stream. *)
    exception EmptyStream

    (* `IndexError` -- Raised when trying to access element which index is out of allowable range. *)
    exception IndexError

    (* `toList` -- Takes all elements from stream to list. *)
    (* Do not use on infinite streams. *)
    fun toList (h >> t) = h :: (toList t)
    |   toList ends     = []

    (* `head` -- Extracts first element of stream. *)
    fun head (h >> _) = h
    |   head ends     = raise EmptyStream

    (* `tail` -- Extracts 'tail' of stream. *)
    (*fun tail (_ >> t) = t
    |   tail ends     = raise EmptyStream*)

    (* `tail` -- [lazy] Extracts 'tail' of stream. *)
    fun lazy tail (_ >> t) = t
    |        tail ends     = raise EmptyStream

    (* `getNth` -- Gets n-th element of stream. *)
    fun getNth _ ends      = raise IndexError
    |   getNth 0 s         = head s
    |   getNth n (h >> t)  = getNth (n - 1) t

    (* `countAll` -- Returns 'length' of stream. *)
    (* Do not use on infinite streams. *)
    fun countAll (h >> t) = 1 + (countAll t)
    |   countAll ends     = 0

    (* `countIf` -- Returns number of elements of stream that are satisfying predicate `p`. *)
    (* Do not use on infinite streams. *)
    fun countIf p =
      let
        fun iter (h >> t) =
          if p h
          then 1 + (iter t)
          else iter t
        |   iter ends     = 0
      in
        iter
      end

    (* `countIf` -- Returns number of elements of stream that are satisfying predicate `p`. *)
    (* Equivalent to `countIf (fn x => x = a)`. *)
    fun count a = countIf (fn x => x = a);

    (* `findIf` -- Returns first element of stream that satisfies predicate `p`. *)
    fun findIf p =
      let
        fun iter (h >> t) =
          if p h
            then SOME h
            else iter t
        |   iter ends     = NONE
      in
        iter
      end

    (* `some` -- Checks if element that satisfies predicate `p` is present in stream. *)
    (* Using on infinite streams is not recommended. *)
    fun some p =
      let
        fun iter (h >> t) = p h orelse iter t
        |   iter ends     = false
      in
        iter
      end

    (* `every` -- Checks if all elements of stream are satisfing predicate `p`. *)
    (* Do not use on infinite streams. *)
    fun every p =
      let
        fun iter (h >> t) = p h andalso iter t
        |   iter ends     = true
      in
        iter
      end

    (* `none` -- Checks if stream contains no elements satisfying predicate `p`. *)
    (* Equivalent to `every (not o p)`. *)
    fun none p = every (not o p)

    (* `contains` -- Check if element `a` is present in stream. *)
    (* Equivalent to `some (fn x => x = a)` *)
    fun contains a = some (fn x => x = a)

    (* `apply` -- Applies function `f` to all elements of stream. *)
    (* Do not use on infinite streams. *)
    fun apply f =
      let
        fun iter (h >> t) = (f h; iter t)
        |   iter ends     = ()
      in
        iter
      end

    (* `transform` -- [lazy] Transforms stream elements using function `f`. *)
    (* Can be used in stream definition. *)
    fun transform f =
      let
        fun lazy iter (h >> t) = (f h) >> (iter t)
        |        iter ends     = ends
      in
        iter
      end

    (* `foldLeft` -- Left fold. *)
    (* Do not use on infinite streams. *)
    fun foldLeft f i =
      let
        fun iter i (h >> t) = iter (f (h, i)) t
        |   iter i ends     = i
      in
        iter i
      end

    (* `foldRight` -- Right fold. *)
    (* Do not use on infinite streams. *)
    fun foldRight f i =
      let
        fun iter (h >> t) = f (h, iter t)
        |   iter ends     = i
      in
        iter
      end

    (* `takeIf` -- [lazy] Take only elements from stream that are satisfying predicate `p`. *)
    (* Do not use in stream definition. *)
    fun takeIf p =
      let
        fun lazy iter (h >> t) =
          if p h
            then h >> (iter t)
            else iter t
        |        iter ends     = ends
      in
        iter
      end

    (* `rejectIf` -- [lazy] Removes all elements from stream that satisfies predicate `p`. *)
    (* Equivalent to `takeIf (not o p)`. *)
    fun rejectIf p = takeIf (not o p)

    (* `reject` -- [lazy] Removes all elements from stream that are equal to `a`. *)
    (* Equivalent to `takeIf (fn x => not (x = a))` *)
    fun reject a = takeIf (fn x => not (x = a))

    (* `rejectFirstIf` -- [lazy] Removes only first element from stream that satisfies predicate `p`. *)
    fun rejectFirstIf p =
      let
        fun lazy iter (h >> t) =
          if p h
            then t
            else h >> (iter t)
        |        iter ends     = ends
      in
        iter
      end

    (* `rejectFirst` -- [lazy] Removes only first element from stream that is equal to `a`. *)
    (* Equivalent to `rejectFirstIf (fn x => not (x = a))` *)
    fun rejectFirst a = rejectFirstIf (fn x => x = a)

    (* `takeN` -- [lazy] Takes `n` first elements from stream (to finite stream). *)
    fun lazy takeN 0 _        = ends
    |        takeN _ ends     = raise IndexError
    |        takeN n (h >> t) = h >> (takeN (n - 1) t)

    (* `takeN` -- [lazy] Takes `n` first elements from stream (to finite stream). *)
    (*fun takeN n =
      let
        fun lazy iter 0 _        = ends
        |        iter _ ends     = raise EmptyStream
        |        iter n (h >> t) = h >> (iter (n - 1) t)
      in
        iter n
      end*)

    (* `takeNToList` - Takes `n` first elements from stream (to list). *)
    fun takeNToList 0 _        = []
    |   takeNToList _ ends     = raise IndexError
    |   takeNToList n (h >> t) = h :: (takeNToList (n - 1) t)

    (* `takeWhile` -- [lazy] Takes elements from stream (to (possibly) finite stream) while they are satisfyng predicate `p`. *)
    fun takeWhile p =
      let
        fun lazy iter (h >> t) =
          if p h
            then h >> (iter t)
            else ends
        |        iter ends     = ends
      in
        iter
      end

    (* `takeWhileToList` -- Takes elements from stream (to list) while they are satisfying predicate `p`. *)
    (* Using on infinite streams is not recommended. *)
    fun takeWhileToList p (h >> t) =
      if p h
        then h :: (takeWhileToList p t)
        else []
    |   takeWhileToList _ ends     = []


    (* `rejectN` -- [lazy] Rejects first `n` elements of stream (to stream). *)
    fun lazy rejectN 0 (h >> t) = h >> t
    |        rejectN _ ends     = raise IndexError
    |        rejectN n (h >> t) = rejectN (n - 1) t

    (* `rejectNToList` -- Rejects first `n` elements of stream (to list). *)
    (* Do not use on infinite streams. *)
    fun rejectNToList 0 (h >> t) = toList (h >> t)
    |   rejectNToList _ ends     = raise IndexError
    |   rejectNToList n (h >> t) = rejectNToList (n - 1) t

    (* `rejectWhile` -- [lazy] Rejects elements of stream (to stream) while they are satisfying predicate p. *)
    fun rejectWhile p =
      let
        fun lazy iter (h >> t) =
          if p h
            then iter t
            else h >> t
        |        iter ends     = ends
      in
        iter
      end

    (* `rejectWhileToList` -- Rejects elements of stream (to list) while they are satisfying predicate `p`. *)
    (* Do not use on infinite streams. *)
    fun rejectWhileToList p (h >> t) =
      if p h
        then rejectWhileToList p t
        else h :: (toList t)
    |   rejectWhileToList _ ends     = []


    (* `concatenate` -- [lazy] Concatenates two streams (first stream should be finite). *)
    fun lazy concatenate u ends              = u
    |        concatenate ends v              = v
    |        concatenate (u >> u') (v >> v') = u >> (concatenate u' (v >> v'))

    (* `concatenate` -- [lazy] Concatenates two streams (first stream should be finite). *)
    (*fun concatenate u =
      let
        fun lazy iter u ends              = u
        |        iter ends v              = v
        |        iter (u >> u') (v >> v') = u >> (iter u' (v >> v'))
      in
        iter u
      end*)

    (* `zip` -- [lazy] Zips two streams using function `f`. *)
    (* The output stream length is defined as smallest of lengths of input streams. *)
    fun zip f =
      let
        fun lazy iter ends _              = ends
        |        iter _ ends              = ends
        |        iter (u >> u') (v >> v') = (f (u, v)) >> (iter u' v')
      in
        iter
      end

    (* `zipToTuple` -- [lazy] Zips two streams (to stream of 2-tuples). *)
    (* Equivalent to `zip (fn (x, y) => (x, y))`. *)
    (* The output stream length is defined as smallest of lengths of input streams. *)
    fun lazy zipToTuple ends _              = ends
    |        zipToTuple _ ends              = ends
    |        zipToTuple (u >> u') (v >> v') = (u, v) >> (zipToTuple u' v')

    (* `interleave` -- [lazy] Interleaves two streams (starting from the first one). *)
    fun lazy interleave u ends              = u
    |        interleave ends v              = v
    |        interleave (u >> u') (v >> v') = u >> (interleave (v >> v') u')

    (* `reverse` -- [lazy] Reverses stream. *)
    (* Do not use on infinite streams. *)
    fun reverse u =
      let
        fun lazy iter i (h >> t) = (iter (h >> i)) t
        |        iter i ends     = i
      in
       iter ends u
      end

    (* `splitIf` -- Splits stream by predicate `p`. *)
    (* Equivalent to `(takeWhile (not o p), rejectWhile p)`. *)
    (* Using on infinite streams is not recommended. *)
    fun splitIf p =
      let
        fun iter u (v >> v') =
          if p v
            then (u, v >> v')
            else
              let
                val lr = iter u v'
              in
                (v >> (#1 lr), #2 lr)
              end
        |   iter u ends      = (u, ends)
      in
        iter ends
      end

    (* `splitIfNot` -- Splits stream by predicate `p`. *)
    (* Equivalent to `splitIf (not o p)`. *)
    (* Equivalent to `(takeWhile p, rejectWhile (not o p))`. *)
    (* Using on infinite streams is not recommended. *)
    fun splitIfNot p = splitIf (not o p)

    (* `partitionBy` -- Partitions stream by predicate `p`. *)
    (* Equivalent to `(takeIf p, rejectIf (not o p))`. *)
    (* Do not use on infinite streams. *)
    (*fun partitionBy p =
      let
        fun iter (l, r) (h >> t) =
          if p h
            then
              iter (h >> l, r) t
            else
              iter (l, h >> r) t
        |   iter (l, r) ends     = (l, r)
      in
        iter (ends, ends)
      end*)

    (* `partitionBy` -- Partitions stream by predicate `p`. *)
    (* Equivalent to `(takeIf p, rejectIf (not o p))`. *)
    (* Do not use on infinite streams. *)
    fun partitionBy p =
      let
        fun iter (h >> t) =
          let
            val lr = iter t
          in
            if p h
              then (h >> (#1 lr), #2 lr)
              else (#1 lr, h >> (#2 lr))
          end
        |   iter ends     = (ends, ends)
      in
        iter
      end

    (* Stream definition example #1 *)
    val rec lazy ones = 1 >> ones

    (* Stream definition example #2 *)
    val rec lazy naturals = 0 >> (transform (fn x => x + 1) naturals)

    (* Stream definition example #3 *)
    (* val lazy test = (print " 1 "; 1) >> (print " 2 "; 2) >> (print " e "; ends); *)
  end;

(* Brings type constructors to top-level environment. *)
val ends = Stream.ends;
val >> = Stream.>>;

(* Cons `>>` operator w/ same precedence as built-in cons operator `::`. *)
infixr 5 >>;
