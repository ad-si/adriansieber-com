---
title: DigitSets - A Haskell Package for Various Digit Alphabets
...

```haskell
data Base1Digit  = U0  -- Unary Digit
data Base2Digit  = B0 | B1  -- Binary Digit
data Base3Digit  = T0 | T1 | T2  -- Ternary Digit
data Base4Digit  = Q0 | Q1 | Q2 | Q3  -- Quaternary Digit
data Base5Digit  = P0 | P1 | P2 | P3 | P4  -- Pental Digit
data Base6Digit  = S0 | S1 | S2 | S3 | S4 | S5  -- Senary Digit
data Base7Digit  = H0 | H1 | H2 | H3 | H4 | H5 | H6  -- Heptal Digit
data Base8Digit  = O0 | O1 | O2 | O3 | O4 | O5 | O6 | O7  -- Octal Digit
data Base9Digit  = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8  -- Nonary Digit

-- Decimal Digit
data Base10Digit = D0 | D1 | D2 | D3 | D4 | D5
                 | D6 | D7 | D8 | D9

-- Duodecimal Digit
data Base12Digit = Z0 | Z1 | Z2 | Z3 | Z4 | Z5
                 | Z6 | Z7 | Z8 | Z9 | ZX | ZE

-- Hexadecimal Digit
data Base16Digit = X0 | X1 | X2 | X3 | X4 | X5
                 | X6 | X7 | X8 | X9 | XA | XB | XC | XD | XE | XF


-- Base 36 Digit (or Senary squared)
data Base36Digit = S00 | S01 | … | S05
                 | S10 | …       | S15
                 …
                 | S50 | …       | S55

-- Base 64 Digit (or Octal squared)
data Base64Digit = O00 | O01 | … | O07
                 | O10 | …       | O17
                 …
                 | O70 | …       | O77

type Uit    = Base1Digit
type Bit    = Base2Digit
type Tit    = Base3Digit
type Qit    = Base4Digit
type Pit    = Base5Digit
type Sit    = Base6Digit
type Hit    = Base7Digit
type Oit    = Base8Digit
type Nit    = Base9Digit
type Dit    = Base10Digit
type DuoDit = Base12Digit
type HexDit = Base16Digit
```

I had several cases where I needed to constraint the number of inputs,
but didn't want to define a new data type,
either because it was to much effort (like base64)
or there were no clear names for the options (storage locations).
With this package I could simply create

```haskell
newtype StorageLocation = StorageLocation Base16Digit
```

alternative

```haskell
newtype StorageLocation = StorageLocation (Mod 16)
```
