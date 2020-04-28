% cat Example.hs
{-# LANGUAGE CPP #-}

#define def(name, sig, impl) name = (impl) :: sig

def(add
, Int -> Int -> Int
, \x y -> x + y
)

main = print $ add 1 2

% ghc -dynamic Example.hs

% ./Example
3
