{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE OverloadedStrings#-}


import qualified Data.Text as T
import qualified Data.Map as M
import Test.Hspec hiding (shouldBe,shouldReturn)
import Test.Hspec.Expectations.Lifted
import HsVerilog
import Text.Shakespeare.Text
import Control.Monad

trimSpace' :: T.Text -> String
trimSpace' x = dropWhile (== ' ') $ trimSpace $ T.unpack x
trimSpace :: String -> String
trimSpace [] = []
trimSpace (' ':'\n':xs) = trimSpace (' ':xs)
trimSpace (' ':'\t':xs) = trimSpace (' ':xs)
trimSpace (' ':' ':xs) = trimSpace (' ':xs)
trimSpace (' ':';':xs) = trimSpace (';':xs)
trimSpace (' ':'@':xs) = trimSpace ('@':xs)
trimSpace (' ':'(':xs) = trimSpace ('(':xs)
trimSpace (' ':')':xs) = trimSpace (')':xs)
trimSpace (' ':',':xs) = trimSpace (',':xs)
trimSpace ('\t':xs) = trimSpace (' ':xs)
trimSpace ('\n':xs) = trimSpace (' ':xs)
trimSpace (x:xs) = x:trimSpace xs

(=~) :: Verilog a => a -> T.Text -> Expectation
(=~) org exp' = (trimSpace' (toVerilog org)) `shouldBe` trimSpace' exp'
infix 1 =~

dff2 :: Circuit
dff2 = circuit "dff2" $ do
  clk <- input "clk" Bit
  rstn <- input "rstn" Bit
  din <- fmap S $ input "din" Bit
  _dout <- output "dout" Bit
  reg' "dout" Bit [Posedge clk,Negedge rstn] $
    If (Not (S rstn))
      0
      din

main :: IO ()
main = hspec $ do
  describe "verilog" $ do
    it "dff and simple register" $
      dff2 =~
      [sbt|module dff2 (clk, rstn, din, dout);
          |  input clk;
          |  input rstn;
          |  input din;
          |  output dout;
          |  reg dout;
          |  always @(posedge clk or negedge rstn) begin
          |    if (!rstn)
          |      dout <= 0;
          |    else
          |      dout <= din;
          |  end
          |endmodule
          |]
    it "counter and simple register" $ do
      counter <- circuitM "counter" $ do
        clk <- input "clk" Bit
        rstn <- input "rstn" Bit
        _ <- output "dout" $ 7><0
        reg "dout" (7><0) [Posedge clk,Negedge rstn] $ \dout ->
          If (Not (S rstn)) 0 $
            If (Eq dout 7) 
              0
              (dout + 1)
      counter =~ 
        [sbt|module counter (clk, rstn, dout);
            |  input  clk;
            |  input  rstn;
            |  output [7:0] dout;
            |  reg [7:0] dout;
            |  always @(posedge clk or negedge rstn) begin
            |    if (!rstn)
            |      dout[7:0] <= 0;
            |    else if ((dout[7:0] == 7))
            |      dout[7:0] <= 0;
            |    else
            |      dout[7:0] <= (dout[7:0] + 1);
            |  end
            |endmodule
            |]
    it "state machine(I think this is dirty.)" $ do
      cir <- circuitM "StateMachine" $ do
        clk <- input "clk" Bit
        rstn <- input "rstn" Bit
        let idx :: String -> Exp
            idx state = (M.fromList $ zip ["init","run0","run1"] (map C [0..])) M.! state
        reg "state" (7><0) [Posedge clk,Negedge rstn] $ \state ->
          If (Not (S rstn)) (idx "init") $
          If (Eq state (idx "init")) (idx "run0") $
          If (Eq state (idx "run0")) (idx "run1") $ idx "init"
      cir =~ 
        [sbt|module StateMachine (clk, rstn);
            |  input  clk;
            |  input  rstn;
            |  reg [7:0] state;
            |  always @(posedge clk or negedge rstn) begin
            |    if (!rstn)
            |      state[7:0] <= 0;
            |    else if ((state[7:0] == 0))
            |      state[7:0] <= 1;
            |    else if ((state[7:0] == 1))
            |      state[7:0] <= 2;
            |    else
            |      state[7:0] <= 0;
            |  end
            |endmodule
            |]
    it "instance" $ do
      testTop <- circuitM "Top" $ do
        clk <- input "clk" Bit
        din <- input "din" Bit
        rstn <- input "rstn" Bit
        dout <- output "dout" Bit
        i0 <- inst dff2 "dff0" [("clk",clk),
                                ("din",din),
                                ("rstn",rstn)]
        i1 <- inst dff2 "dff1" [("clk",clk),
                                ("din",i0.:"dout"),
                                ("rstn",rstn)]
        assign dout (S (i1.:"dout"))
      testTop =~ 
        [sbt|module Top (clk, din, rstn, dout);
            |  input  clk;
            |  input  din;
            |  input  rstn;
            |  output  dout;
            |  wire  dff0_dout;
            |  wire  dff1_dout;
            |  assign dout = dff1_dout
            |  dff2 dff0(.clk(clk),
            |            .din(din),
            |            .rstn(rstn),
            |            .dout(dff0_dout));
            |  dff2 dff1(.clk(clk),
            |            .din(dff0_dout),
            |            .rstn(rstn),
            |            .dout(dff1_dout));
            |endmodule
            |]
    it "instance loop" $ do
      testTop <- circuitM "Top" $ do
        clk <- input "clk" Bit
        rstn <- input "rstn" Bit
        i0 <- inst dff2 "dff0" [("clk",clk),
                                ("rstn",rstn)]
        i1 <- inst dff2 "dff1" [("clk",clk),
                                ("rstn",rstn)]
        connect i0 "din" $ i1.:"dout"
        connect i1 "din" $ i0.:"dout"
      testTop =~ 
        [sbt|module Top (clk, rstn);
            |  input  clk;
            |  input  rstn;
            |  wire  dff0_dout;
            |  wire  dff1_dout;
            |  dff2 dff0(.clk(clk),
            |            .rstn(rstn),
            |            .dout(dff0_dout),
            |            .din(dff1_dout));
            |  dff2 dff1(.clk(clk),
            |            .rstn(rstn),
            |            .dout(dff1_dout),
            |            .din(dff0_dout));
            |endmodule
            |]
    it "assign" $ do
      cir <- circuitM "mulAdd" $ do
        din0 <- fmap S $ input "din0" Bit
        din1 <- fmap S $ input "din1" Bit
        din2 <- fmap S $ input "din2" Bit
        dout <- output "dout" Bit
        assign dout $ (din0 * din1) + din2
      cir =~
        [sbt|module mulAdd (din0, din1, din2, dout);
            |  input  din0;
            |  input  din1;
            |  input  din2;
            |  output dout;
            |  assign dout = ((din0 * din1) + din2)
            |endmodule
            |]
    it "range" $ do
      cir <- circuitM "mulAdd" $ do
        din0 <- fmap S $ input "din0" $ 7><0
        din1 <- fmap S $ input "din1" $ 7><0
        din2 <- fmap S $ input "din2" $ 7><0
        dout <- output "dout" $ 7><0
        assign dout $ din0 * din1 + din2
      cir =~
        [sbt|module mulAdd (din0, din1, din2, dout);
            |  input [7:0] din0;
            |  input [7:0] din1;
            |  input [7:0] din2;
            |  output [7:0] dout;
            |  assign dout[7:0] = ((din0[7:0] * din1[7:0]) + din2[7:0])
            |endmodule
            |]
  describe "simulation: Exp" $ do
  --   it "Add" $ do
  --      val (initCircuit "hoge") (2 + 1) `shouldBe` 3
  --   it "Mul" $
  --      val (initCircuit "hoge") (2 * 2) `shouldBe` 4
    it "count up" $ do
      print "123"
      counter <- circuitM "counter" $ do
        clk <- input "clk" Bit
        rstn <- input "rstn" Bit
        _ <- output "dout" $ 7><0
        reg "dout" (7><0) [Posedge clk,Negedge rstn] $ \dout ->
          If (Not (S rstn)) 0 $
            If (Eq dout 7) 
              0
              (dout + 1)
      simM counter $ do
        readInput "rstn" `shouldReturn` 0
        "rstn" <== 1
        readInput "rstn" `shouldReturn` 1
        readReg "dout" `shouldReturn` 0
        updateReg
        readReg "dout" `shouldReturn` 1
        updateReg
        readReg "dout" `shouldReturn` 2
        updateReg
        readReg "dout" `shouldReturn` 3
        updateReg
        readReg "dout" `shouldReturn` 4
        print'
      return ()
