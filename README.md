# HsVerilog: Synthesizable Verilog DSL supporting for multiple clock and reset

[![Hackage version](https://img.shields.io/hackage/v/hsverilog.svg?style=flat)](https://hackage.haskell.org/package/hsverilog)  [![Build Status](https://travis-ci.org/junjihashimoto/hsverilog.png?branch=master)](https://travis-ci.org/junjihashimoto/hsverilog) [![Coverage Status](https://coveralls.io/repos/junjihashimoto/hsverilog/badge.png)](https://coveralls.io/r/junjihashimoto/hsverilog)

## Getting started

Install this from Hackage.

    cabal update && cabal install hsverilog

## Usage

Syntax is similar to Verilog.
See tests/test.hs and following examples.


### counter circuit

```
circuit "counter" $ do
  clk <- input "clk" Bit
  rstn <- input "rstn" Bit
  _ <- output "dout" $ 7><0
  reg "dout" (7><0) [Posedge clk,Negedge rstn] $ \dout ->
    If (Not (S rstn)) 0 $
      If (Eq dout 7) 
        0
        (dout + 1)
```
