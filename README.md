# Stream.SML
Lazy list (*) implementation for [SML/NJ](http://www.smlnj.org/).

Requires SML/NJ ver. 110.50 or later.

(*) aka Infinite list or Stream

# Usage examples
## Sieve of Eratosphenes (naïve variant)
```sml
fun sieve (a, b) =
  let
    open Stream

    (* `naturalsFrom2` -- Stream of <2..>. *)
    val addOne = transform (fn n => n + 1)
    val rec lazy naturalsFrom2 = 2 >> (addOne naturalsFrom2)

    fun divides m n = n mod m = 0

    (* `sieve` -- Removes all multipliers of `h` from stream. *)
    fun lazy sieve (h >> t) = h >> (sieve (rejectIf (divides h) t))
    |        sieve ends     = ends

    val lessThanB = takeWhile (fn x => x <= b)
    val greaterThanA = rejectWhile (fn x => x < a)

    (* `primesInRange` -- Stream of primes bounded by [a; b]. *)
    val primesInRange = greaterThanA (lessThanB (sieve naturalsFrom2));
  in
    toList primesInRange
  end;

(* 23, 29, 31, 37, 41, 43, 47. *)
val primes = sieve (20, 50);
```

## Sieve of Eratosphenes (optimized variant)
```sml
fun sieve (a, b) =
  let
    open Stream

    (* `naturalsFrom2` -- Stream of <2..>. *)
    val addOne = transform (fn n => n + 1)
    val rec lazy naturalsFrom2 = 2 >> (addOne naturalsFrom2)

    fun divides m n = n mod m = 0

    (* `sieve` -- Removes all multipliers of `h` starting from `h * h` from stream. *)
    fun lazy sieve (h >> t) =
      if h * h < b
        then
          let
            val (sieved, rest) = splitIfNot (fn x => x < h * h) t
            val filtered = rejectIf (divides h) rest
          in
            h >> (sieve (concatenate sieved filtered))
          end
        else
          h >> t
    |        sieve ends        = ends

    val lessThanB = takeWhile (fn x => x <= b)
    val greaterThanA = rejectWhile (fn x => x < a)

    (* `primesInRange` -- Stream of primes bounded by [a; b]. *)
    val primesInRange = greaterThanA (lessThanB (sieve naturalsFrom2));
  in
    toList primesInRange
  end;

(* 23, 29, 31, 37, 41, 43, 47. *)
val primes = sieve (20, 50);
```

# Available functions
See `stream.sml`.
