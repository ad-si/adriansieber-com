# Comparison of Compile-to-JavaScript Languages

## Human Readable JavaScript

### Gleam

Website: https://gleam.run
Playground: https://tour.gleam.run

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

Website: https://rescript-lang.org
Playground: https://rescript-lang.org/try

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

Website: https://purescript.org
Playground: https://try.purescript.org

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

Website: https://elm-lang.org
Playground: https://ellie-app.com

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

Website: https://gren-lang.org


### OCaml

Website: https://ocaml.org
Playground: https://melange.re/v5.0.0/playground

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


### F#

Website: https://fable.io
Playground: https://fable.io/repl

```fsharp
let add num1 num2 =
  num1 + num2
```

```js
export function add(num1, num2) {
    return num1 + num2;
}
```


### V Lang

Website: https://vlang.io
Playground: https://play.vlang.io
Docs: https://docs.vlang.io/debugging.html#javascript-backend

From https://play.vlang.io/p/88b5d16294:
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

Website: https://lean-lang.org

> [!WARNING]
> Does currently not work:
> https://github.com/leanprover/lean4/issues/398


```sh
brew install elan
elan default leanprover/lean4:stable
```


### Idris

https://docs.idris-lang.org/en/latest/reference/codegen.html#javascript

```lean
def add (num1 : Nat) (num2 : Nat) : Nat :=
      num1 + num2
```


### Agda

Website: https://wiki.portal.chalmers.se/agda/pmwiki.php
Docs: https://agda.readthedocs.io/en/v2.5.2/tools/compilers.html#javascript-backend


## WASM

### Rust

Website: https://www.rust-lang.org


### Haskell

Website: https://www.haskell.org


### Futhark

Website: https://futhark-lang.org
Docs: https://futhark.readthedocs.io/en/latest/js-api.html


### Luau

Website: https://luau.org
Playground: https://luau.org/demo
