{-#LANGUAGE OverloadedStrings#-}
module HsVerilog.Library where

import HsVerilog.Type
import HsVerilog.Verilog

dff :: Circuit
dff = circuit "dff" $ do
  clk <- input "clk" Bit
  rstn <- input "rstn" Bit
  din <- input "din" Bit
  _dout <- output "dout" Bit
  reg' "dout" Bit [Posedge clk,Negedge rstn] $
    If (Not (S rstn))
      (C 0)
      (S din)

dff8 :: Circuit
dff8 = circuit "dff8" $ do
  clk <- input "clk" Bit
  rstn <- input "rstn" Bit
  din <- input "din" $ Range 7 0
  _dout <- output "dout" $ Range 7 0
  reg' "dout" (Range 7 0) [Posedge clk,Negedge rstn] $
    If (Not (S rstn))
      (C 0)
      (S din)

