# invertible-syntax

[![Build Status](https://travis-ci.org/mossprescott/invertible-syntax.svg)](https://travis-ci.org/mossprescott/invertible-syntax)

With Invertible Syntax, you write a description of your language similar to parser combinators, and then can you use that same description to parse or to pretty-print. The results are guaranteed to be (partially) isomorphic by construction.

Inspired by Haskell's [invertible-syntax](http://hackage.haskell.org/package/invertible-syntax) and [partial-isomorphisms](http://hackage.haskell.org/package/partial-isomorphisms).

## Getting Started

A port of the simple parser presented in the paper is in [simple.scala](src/test/scala/invertible/simple.scala).

A more complicated example parser, handling a richer language and providing source locations, is in [example.scala](src/test/scala/invertible/example.scala).
