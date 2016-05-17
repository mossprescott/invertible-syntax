# invertible-syntax

[![Build Status](https://travis-ci.org/mossprescott/invertible-syntax.svg)](https://travis-ci.org/mossprescott/invertible-syntax)

With Invertible Syntax, you write a description of your language similar to parser combinators, and then can you use that same description to parse or to pretty-print. The results are guaranteed to be (partially) isomorphic by construction.

Inspired by Haskell's [invertible-syntax](http://hackage.haskell.org/package/invertible-syntax) and [partial-isomorphisms](http://hackage.haskell.org/package/partial-isomorphisms).

## Getting Started

Define your syntax using the combinators of `Syntax[P]`. Accepting the actual instance as a parameter allows the same function to be used to define a parser or pretty-printer, depending on what instance it is supplied with:

```
scala> import invertible._, Syntax._

scala> def ints[P[_]](implicit S: Syntax[P]) = int.sepBy1(sepSpace)
ints: [P[_]](implicit S: invertible.Syntax[P])P[List[BigInt]]

```

To get a parser, hand it to `Syntax.parser`. If the parser fails, you get a nice error message identifying the location of the problem:

```
scala> val p = parser(ints(_))
p: String => scalaz.\/[invertible.ParseFailure,List[BigInt]] = <function1>

scala> p("1 20 300")
res12: scalaz.\/[invertible.ParseFailure,List[BigInt]] = \/-(List(1, 20, 300))

scala> p("1 abc").leftMap(println)
expected: " " or digit; found: 'a'
1 abc
  ^
```

To get a printer, use `Syntax.printer`:

```
scala> val pp = printer(ints(_))
pp: List[BigInt] => Option[scalaz.Cord] = <function1>

scala> pp(List[BigInt](1, 20, 300))
res17: Option[scalaz.Cord] = Some(1 20 300)
```

### Examples

A port of the simple parser presented in the paper is in [simple.scala](src/test/scala/invertible/simple.scala).

A more complicated example parser, handling a richer language and providing source locations, is in [example.scala](src/test/scala/invertible/example.scala).
