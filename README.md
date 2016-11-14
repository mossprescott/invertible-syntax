# invertible-syntax

[![Build Status](https://travis-ci.org/mossprescott/invertible-syntax.svg)](https://travis-ci.org/mossprescott/invertible-syntax)

With Invertible Syntax, you write a description of your language similar to what you would write with parser combinators, and then can you use that same description to parse or to pretty-print. The results are guaranteed to be isomorphic by construction.

Inspired by Haskell's [invertible-syntax](http://hackage.haskell.org/package/invertible-syntax) and [partial-isomorphisms](http://hackage.haskell.org/package/partial-isomorphisms).

## Getting Started

Define your syntax by implementing `Syntax[A]` for your desired type. The `apply` method will be provided with a type for which the `Transcriber` typelass is defined, and uses the primitives and combinators defined in the `Syntax` companion to construct a `P[A]`.

```
scala> import invertible._, Syntax._

scala> val ints = new Syntax[List[BigInt]] {
     |   def apply[P[_]: Transcriber] = int sepBy1 sepSpace
     | }
ints: invertible.Syntax[List[BigInt]] = $anon$1@506cb8df
```

To parse input text, simply call `parse`. If the parser fails, you get a nice error message identifying the location of the problem:

```
scala> ints.parse("1 20 30")
res0: scalaz.\/[invertible.ParseFailure,List[BigInt]] = \/-(List(1, 20, 30))

scala> ints.parse("1 abc").leftMap(println)
expected: " " or digit; found: 'a'
1 abc
  ^
```

To print:

```
scala> ints.print(List(1, 20, 300))
res2: Option[String] = Some(1 20 300)
```

Note: in a simple syntax like this, printing never fails because the syntax can represent every possible value. But it’s not uncommon for the syntax’s result type to include values that can’t be represented in the textual form. E.g. variable names may be arbitrary `String`s according to the AST, but only alphanumeric names are actually parsed/printed. In that case, the printer will be unable to map the value to a valid text, and will return `None`.

### Examples

A port of the simple parser presented in [Tillmann Rendel and Klaus Ostermann. Invertible syntax descriptions: Unifying parsing and pretty printing. Haskell symposium, 2010](http://www.informatik.uni-marburg.de/~rendel/unparse/) is in [simple.scala](src/test/scala/invertible/simple.scala).

A more complicated example parser, handling a richer language and providing source locations, is in [example.scala](src/test/scala/invertible/example.scala).
