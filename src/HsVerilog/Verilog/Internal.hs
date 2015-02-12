{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE OverloadedStrings#-}

module HsVerilog.Verilog.Internal where

import Text.Shakespeare.Text
import Prelude hiding (exp)
import HsVerilog.Type
import qualified Data.Text as T
--import qualified Data.Text.IO as T
--import Control.Monad.Trans.State
--import Control.Monad.Trans.Reader
--import Control.Monad
import Data.Monoid
import Data.List
import qualified Data.Map as M
--import HsVerilog.Verilog

instance ToText Integer where
  toText = toText.show

instance Verilog Range where
  toVerilog (Range f t) = [st|[#{f}:#{t}]|]
  toVerilog (RangeBit f) = [st|[#{f}]|]
  toVerilog Bit = ""

instance Verilog Exp where
  toVerilog (If a b c) = [st|
  if (#{toVerilog a})
    #{toVerilog b}
  else
    #{toVerilog c}
|]
  toVerilog (Mux a b c) = [st|(#{toVerilog a}?#{toVerilog b}:#{toVerilog c})|]
  toVerilog (Not a)     = [st|!#{toVerilog a}|]
  toVerilog (Or a b)    = [st|(#{toVerilog a} || #{toVerilog b})|]
  toVerilog (BitOr a b) = [st|(#{toVerilog a} | #{toVerilog b})|]
  toVerilog (And a b)   = [st|(#{toVerilog a} && #{toVerilog b})|]
  toVerilog (BitAnd a b)= [st|(#{toVerilog a} & #{toVerilog b})|]
  toVerilog (Add a b)   = [st|(#{toVerilog a} + #{toVerilog b})|]
  toVerilog (Mul a b)   = [st|(#{toVerilog a} * #{toVerilog b})|]
  toVerilog (Div a b)   = [st|(#{toVerilog a} / #{toVerilog b})|]
  toVerilog (Eq a b)    = [st|(#{toVerilog a} == #{toVerilog b})|]
  toVerilog (S (Signal name range _))= [st|#{name}#{toVerilog range}|]
  toVerilog (C v)       = [st|#{v}|]
  toVerilog (NonBlockAssign a b) = [st|    #{toVerilog a} <= #{toVerilog b};|]
  toVerilog (BlockAssign a b)    = [st|#{toVerilog a} = #{toVerilog b};|]

nonblockExp' sig a@(If _ _ _) = nonblockExp sig a
nonblockExp' sig a = (NonBlockAssign sig a)

nonblockExp sig (If a b@(If _ _ _) c@(If _ _ _)) = 
  If a (nonblockExp sig b) (nonblockExp sig c) 
nonblockExp sig (If a b c@(If _ _ _)) =
  If a (NonBlockAssign sig b) (nonblockExp sig c)
nonblockExp sig (If a b@(If _ _ _) c) =
  If a (nonblockExp sig b) (NonBlockAssign sig c)
nonblockExp sig (If a b c) =
  If a (NonBlockAssign sig b) (NonBlockAssign sig c)
nonblockExp sig a = a

instance Verilog Stim where
  toVerilog (Posedge sig) = [st|posedge #{sname sig}|]
  toVerilog (Negedge sig) = [st|negedge #{sname sig}|]

instance Verilog Always where
  toVerilog (Always sig stims exp) = [st|
  always @(#{stimstr}) begin
#{toVerilog nbexp}
  end
|]
    where
      stimstr = T.intercalate " or " $ map (\stim -> toVerilog stim) $ stims
      nbexp = nonblockExp' (S sig) exp

instance Verilog Assign where
  toVerilog (Assign sig exp) = [st|  assign #{toVerilog (S sig)} = #{toVerilog exp}|]

-- instance Verilog Instance where
--   toVerilog (Instance name cir connects) = [st|  #{cname cir} #{name}(#{conn});|]
--     where
--       conn = T.intercalate ",\n" $ map (\((Signal fn frange _),(Signal tn trange _))-> [st|.#{fn}(#{tn}#{toVerilog trange})|]) $ connects

instance Verilog Circuit where
  toVerilog cir = [st|
module #{cname cir} (#{portlist cir});
#{iplist cir}#{oplist cir}#{inoutplist cir}
#{wirelist cir}
#{reglist cir}
#{assignlist cir}
#{alwayslist cir}
#{instlist cir}
endmodule
|]

portList :: Circuit -> [Signal]
portList cir = cinput cir ++ coutput cir

portMap :: Circuit -> M.Map T.Text Signal
portMap cir = M.fromList $ map (\sig -> (sname sig,sig)) (portList cir)

portlist :: Circuit -> T.Text
portlist cir  = T.intercalate ", " $ map sname $ portList cir

iplist :: Circuit -> T.Text
iplist cir   = T.unlines $ map (\(Signal n range _) -> [st|  input #{toVerilog range} #{n};|]) $ cinput cir

oplist :: Circuit -> T.Text
oplist cir    = T.unlines $ map (\(Signal n range _) -> [st|  output #{toVerilog range} #{n};|])$ coutput cir

inoutplist :: Circuit -> T.Text
inoutplist cir = T.unlines $ map (\(Signal n range _) -> [st|  inout #{toVerilog range} #{n};|]) $ cinout cir

reglist :: Circuit -> T.Text
reglist cir  = T.unlines $ map (\(Signal n range _) -> [st|  reg #{toVerilog range} #{n};|])   $ map alsig $ creg cir

instOutputPort :: T.Text -> Circuit -> [(Signal,Signal)]
instOutputPort name cir =
  let op  = coutput cir
      ren v = (v,v{sname=name <> "_" <> sname v})
  in map ren op

wireToVerilog :: Signal -> T.Text
wireToVerilog (Signal n range _) = [st|  wire #{toVerilog range} #{n};|]

wireSignals :: Circuit -> [Signal]
wireSignals cir =
  let assignWire = (fmap assig $ cassign cir) \\ (coutput cir)
      instWire = map snd $ concat $ map (\inst -> instOutputPort (iname inst) (icircuit inst)) (cinstance cir)
  in assignWire ++ instWire

wirelist :: Circuit -> T.Text
wirelist cir = T.intercalate "\n" $ fmap wireToVerilog $ wireSignals cir

alwayslist cir = T.unlines $ map toVerilog $ creg cir

assignlist cir = T.unlines $ map toVerilog $ cassign cir

instToVerilog :: Circuit -> Instance -> T.Text
instToVerilog cir inst = [st|  #{cname (icircuit inst)} #{name}(#{conn});|]
  where
    name = iname inst
    conn = T.intercalate ",\n" $ map (\((Signal fn frange _),(Signal tn trange _))-> [st|.#{fn}(#{tn}#{toVerilog trange})|]) $ cinstanceConnect cir M.! name

instlist :: Circuit -> T.Text
instlist cir = T.unlines $ map (\ist -> instToVerilog cir ist) $ cinstance cir
