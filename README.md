# invertible-syntax

[![Build Status](https://travis-ci.org/mossprescott/invertible-syntax.svg)](https://travis-ci.org/mossprescott/invertible-syntax)

With Invertible Syntax, you write a description of your language similar to what you would write with parser combinators, and then can you use that same description to parse or to pretty-print. The results are guaranteed to be isomorphic by construction.

Inspired by Haskell's [invertible-syntax](http://hackage.haskell.org/package/invertible-syntax) and [partial-isomorphisms](http://hackage.haskell.org/package/partial-isomorphisms).

## Getting Started

Define your syntax using the combinators of `Syntax[P]`. Accepting the actual instance as a parameter allows the same function to be used to define a parser or pretty-printer, depending on what instance it is supplied with:

```
scala> import invertible._

scala> def ints[P[_]]: Syntax[P, List[BigInt]] = { S =>
     |   import S._
     |   int sepBy1 sepSpace
     | }
ints: [P[_]]=> invertible.Syntax[P,List[BigInt]]

```

To get a parser, hand it to `Syntax.parser`. If the parser fails, you get a nice error message identifying the location of the problem:

```
scala> val p = Syntax.parser(ints)
p: invertible.Syntax.Parser[List[BigInt]] = <function1>

scala> p("1 20 300")
res0: scalaz.\/[invertible.ParseFailure,List[BigInt]] = \/-(List(1, 20, 300))

scala> p("1 abc").leftMap(println)
expected: " " or digit; found: 'a'
1 abc
  ^
```

To get a printer, use `Syntax.printer`:

```
scala> val pp = Syntax.printer(ints(_))
pp: invertible.Syntax.Printer[List[BigInt]] = <function1>

scala> pp(List[BigInt](1, 20, 300))
res1: Option[scalaz.Cord] = Some(1 20 300)
```

### Examples

A port of the simple parser presented in [Tillmann Rendel and Klaus Ostermann. Invertible syntax descriptions: Unifying parsing and pretty printing. Haskell symposium, 2010](http://www.informatik.uni-marburg.de/~rendel/unparse/) is in [simple.scala](src/test/scala/invertible/simple.scala).

A more complicated example parser, handling a richer language and providing source locations, is in [example.scala](src/test/scala/invertible/example.scala).
