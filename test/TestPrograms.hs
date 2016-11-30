{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module TestPrograms where

import NeatInterpolation

in1, in2, in3, in4 :: String

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
      else
        y := false;
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


testPrograms :: [(String, String)]
testPrograms = [("in1", in1), ("in2", in2), ("in3", in3), ("in4", in4)]