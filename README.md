# Stream
Lazy list (aka Infinite list or Stream) implementation for [SML/NJ](http://www.smlnj.org/).
Requires SML/NJ ver. 110.50 or later.

# Usage
## Sieve of Eratosphenes (naïve variant)
```sml
fun sieve (a, b) =
  let
    open Stream

    val addOne = transform (fn n => n + 1)
    val rec lazy naturalsFrom2 = 2 >> (addOne naturalsFrom2)

    fun divides m n = n mod m = 0

    fun lazy sieve (h >> t) = h >> (sieve (rejectIf (divides h) t))
    |        sieve ends     = ends

    val lessThanB = takeWhile (fn x => x <= b)
    val greaterThanA = rejectWhile (fn x => x < a)

    val primesInRange = greaterThanA (lessThanB (sieve naturalsFrom2));
  in
    toList primesInRange
  end;

(* 23, 29, 31, 37, 41, 43, 47. *)
val primes = sieve (20, 50);
```

# Available functions
See `stream.sml`.
