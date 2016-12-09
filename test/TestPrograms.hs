{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module TestPrograms where

import NeatInterpolation
import Text.Regex

in1, in2, in3, in4, in5, in6, in7, in8, in9, in10, in11, in12, in13 :: String

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
    }
    else {
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
    x := a * b;
    while false do {
      a := 20 + a;
      c := c - 1;
    }
    z := 20 * 30;
    a := 20;
    u := a * b;|]

in7 =
  [string|
    x := 2;
    if true then {
      while false do {
        output x;
        skip;
      }
      skip;
    }
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

in12 =
  [string|
    a := (20 * 3) - 1;
    b := a + 9;
    c := 0;
    while b < 68 do {
      output a * b;
      if (10 + a) > 41 then
        output c;
      else {
        c := c + 1;
        b := a;
      }
    }
    output c + a;
  |]

in13 =
  [string|
    x := input;
    while x > 1 do {
      y := x * 2;
      if y > 3 then
        x := x - y;
      else
        skip;
      z := x - 4;
      if z > 0 then
        x := x * 2;
      else
        skip;
      z := z - 1;
    }
    output x;
  |]

testPrograms :: [(String, String)]
testPrograms =
  let progs =  [ ("in1", in1), ("in2", in2), ("in3", in3), ("in4", in4), ("in5", in5)
               , ("in6", in6), ("in7", in7), ("in8", in8), ("in9", in9), ("in10", in10)
               , ("in11", in11), ("in12", in12), ("in13", in13)
               ]
      rgx = mkRegex "(\r\n)|\r|\n"
  in  map (\(n,p) -> (n, subRegex rgx p "\n")) progs

