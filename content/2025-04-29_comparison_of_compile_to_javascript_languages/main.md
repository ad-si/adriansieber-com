# Comparison of Compile-to-JavaScript Languages

## Human Readable JavaScript

### Gleam

Website: [gleam.run](https://gleam.run) \
Playground: [tour.gleam.run](https://tour.gleam.run)

```sh
gleam new my-app
```

Add `target = "javascript"` to `gleam.toml`

```gleam
pub fn add(num1, num2) {
  num1 + num2
}
```

```js
export function add(num1, num2) {
  return num1 + num2;
}
```


**Pros**

- Very clean JavaScript output


### ReScript

Website: [rescript-lang.org](https://rescript-lang.org) \
Playground: [rescript-lang.org/try](https://rescript-lang.org/try)

```rescript
let add = (num1, num2) =>
  num1 + num2
```

```js
function add(num1, num2) {
  return num1 + num2 | 0;
}

export {
  add ,
}
```


### PureScript

Website: [purescript.org](https://purescript.org) \
Playground: [try.purescript.org](https://try.purescript.org)

```sh
purs init
```

```purs
module Add where

import Prelude

add :: Int -> Int -> Int
add num1 num2 =
  num1 + num2
```

```js
var add = function (num1) {
    return function (num2) {
        return num1 + num2 | 0;
    };
};
export {
    add
};
```


### Elm

Website: [elm-lang.org](https://elm-lang.org) \
Playground: [ellie-app.com](https://ellie-app.com)

```elm
module Add exposing (..)

add : Int -> Int -> Int
add num1 num2 =
  num1 + num2
```

```js
var $author$project$Add$add = F2(
	function (num1, num2) {
		return num1 + num2;
	});
```


### Gren

Website: [gren-lang.org](https://gren-lang.org)


### OCaml

Website: [ocaml.org](https://ocaml.org) \
Playground: [melange.re/v5.0.0/playground](https://melange.re/v5.0.0/playground)

```ocaml
let add num1 num2 =
  num1 + num2
```

```js
function add(num1, num2) {
  return num1 + num2 | 0;
}

export {
  add,
}
```

Alternatives:
- [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml)
- [Bonsai](https://opensource.janestreet.com/bonsai/)


### F#

Website: [fable.io](https://fable.io) \
Playground: [fable.io/repl](https://fable.io/repl)

```fsharp
let add num1 num2 =
  num1 + num2
```

```js
export function add(num1, num2) {
    return num1 + num2;
}
```


### Scala

Website: [scala-js.org](https://www.scala-js.org) \
Playgrounds:
  - [scribble.ninja](https://scribble.ninja)
  - [Scastie](https://scastie.scala-lang.org)

```scala
def add(num1: Int, num2: Int): Int = {
  return num1 + num2
}
```

```js
todo
```


### V Lang

Website: [vlang.io](https://vlang.io) \
Playground: [play.vlang.io](https://play.vlang.io) \
Docs: [docs.vlang.io/debugging#javascript-backend](
  https://docs.vlang.io/debugging.html#javascript-backend)

From [play.vlang.io/p/88b5d16294](https://play.vlang.io/p/88b5d16294):
```v
fn add(num1 int, num2 int) int {
  return num1 + num2
}
```

```js
function main__add(num1, num2) {
	try {
		return new int( num1.valueOf() + num2.valueOf());
	} catch (e) {
		if (e instanceof ReturnException) { return e.val; }
		throw e;
	}
}
```


### Lean

Website: [lean-lang.org](https://lean-lang.org)

> [!WARNING]
> Does currently not work:
> https://github.com/leanprover/lean4/issues/398


```sh
brew install elan
elan default leanprover/lean4:stable
```


### Idris

Docs: [docs.idris-lang.org/en/latest/reference/codegen.html#javascript](
  https://docs.idris-lang.org/en/latest/reference/codegen.html#javascript)

```lean
def add (num1 : Nat) (num2 : Nat) : Nat :=
      num1 + num2
```


### Agda

Website: [wiki.portal.chalmers.se/agda/pmwiki.php](https://wiki.portal.chalmers.se/agda/pmwiki.php) \
Docs: [agda.readthedocs.io/en/v2.5.2/tools/compilers.html#javascript-backend](
  https://agda.readthedocs.io/en/v2.5.2/tools/compilers.html#javascript-backend)


### Koka

Website: [koka-lang.github.io](https://koka-lang.github.io) \
Docs: [koka-lang.github.io/koka/doc/book](https://koka-lang.github.io/koka/doc/book.html)

```sh
koka --target=js koka/add.kk
```

```kk
fun add( num1 : int, num2 : int )
  num1 + num2
```

```js
todo
```


## WASM

### Rust

Website: [rust-lang.org](https://www.rust-lang.org)


### Haskell

Website: [haskell.org](https://www.haskell.org)


### Futhark

Website: [futhark-lang.org](https://futhark-lang.org) \
Docs: [futhark.readthedocs.io/en/latest/js-api](
  https://futhark.readthedocs.io/en/latest/js-api.html)


### Luau

Website: [luau.org](https://luau.org) \
Playground: [luau.org/demo](https://luau.org/demo)
