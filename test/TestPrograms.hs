{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module TestPrograms where

import NeatInterpolation

in1, in2, in3, in4, in5, in6, in7, in8, in9, in10, in11 :: String

in1 =
  [string|
  x := a * b;
  if (a * b) > (20 * c) then
    y := 20 + a;
  else
    y := 30 + c;
  z := 20 * 30;
  a := 20;
  u := a * b;|]

in2 =
  [string|
    x := a * b;
    while (20 * c) > (a * b) do {
      a := 20 + a;
      c := c - 1;
    }
    z := 20 * 30;
    a := 20;
    u := a * b;|]

in3 =
  [string|
    x := 2 < 10;
    if x then
      if false then
        y := true;
      else {
        y := false;
        z := 42 * 42;
      }
    else
      y := 100 - (10 * 10);|]

in4 =
  [string|
    x := 2 < 10;
    if x then
      if false then
        output y;
      else
        output y * ((10 + 2) - 1);
    else
      y := 100 - (10 * 10);|]

in5 =
  [string|
    skip;
    skip;
    x := 2 * 20;
    if x < 41 then {
      output x;
      i := 10;
      while i > 0 do
        output x;
      skip;
    } else {
      w := false;
      skip;
    }
    skip;
    output 10;
    skip;
    output 2;
  |]

in6 =
  [string|
    x := 10;
    y := x * 2;|]

in7 =
  [string|
    x := 2;
    if z < x then
      y := 10;
    else
      y := 42;
    output y;|]

in8 =
  [string|
    x := 3;
    if true then
      y := 10;
    else {
      y := 42;
      output y;
    }
    output y;|]

in9 =
  [string|
    x := 10;
    y := 11;
    u := x * (10 - y);
    v := x * input;
    if x < 10 then
      z := 12;
    else
      z := 24;
    output z;|]

in10 =
  [string|
    x := 0;
    y := 2;
    while x < 10 do {
      x := x + 1;
      y := y * y;
    }
    output y;|]

in11 =
  [string|
    x := 0;
    c := x < 10;
    if c then
      output x;
    else
      output c;|]

testPrograms :: [(String, String)]
testPrograms = [ ("in1", in1), ("in2", in2), ("in3", in3), ("in4", in4), ("in5", in5)
               , ("in6", in6), ("in7", in7), ("in8", in8), ("in9", in9), ("in10", in10)
               , ("in11", in11)
               ]

