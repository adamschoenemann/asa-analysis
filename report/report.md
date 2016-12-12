# Automatic Software Analysis - A General Framework
By Oscar Toro and Adam Schønemann

## Introduction
This report will elaborate on the design and implementation of a general framework
for defining automatic software analyses on a small toy-programming language (C--).
The framework is implemented in Haskell.

## Overview
Software analysis is the process of taking a program as input, analysing that program
and give an approximation of a property of that program. This approximation can in
turn be used to optimize the program, guide the programmer, or warn the programmer
about potential errors.

### The language
In order to to analyze a program, one needs a concrete representation of that program
as data. The program we'll be analysing in this report, is the simple program called
C--.
The syntax for C-- is defined by the following grammar:
![c--](./imgs/c--.jpg)

