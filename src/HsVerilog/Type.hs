{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE OverloadedStrings#-}

module HsVerilog.Type where

import qualified Data.Text as T
import qualified Data.Map as M

class Verilog a where
  toVerilog :: a -> T.Text

data Range =
    Range Integer Integer
  | RangeBit Integer
  | Bit
  deriving (Show,Read,Eq)

data Signal = Signal {
  sname :: T.Text
, sbits :: Range
, sval :: Integer
} deriving (Show,Read,Eq)

type InstanceName = T.Text

data Instance = Instance {
  iname :: InstanceName
, icircuit :: Circuit  
} deriving (Show,Read,Eq)

data Stim =
    Posedge Signal
  | Negedge Signal
  deriving (Show,Read,Eq)

data Always = Always {
  alsig :: Signal
, alstim :: [Stim]
, alexp  :: Exp
} deriving (Show,Read,Eq)

data Assign = Assign {
  assig :: Signal
, asexp :: Exp  
} deriving (Show,Read,Eq)

data Exp = 
    If Exp Exp Exp
  | Mux Exp Exp Exp
  | Not Exp
  | Or Exp Exp
  | BitOr Exp Exp
  | And Exp Exp
  | BitAnd Exp Exp
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Eq Exp Exp
  | S Signal
  | C Integer
  | NonBlockAssign Exp Exp
  | BlockAssign Exp Exp
  deriving (Show,Read,Eq)

instance Num Exp where
  fromInteger a = C a
  (+) a b = Add a b
  (-) a b = Sub a b
  (*) a b = Mul a b

data Circuit = Circuit {
  cname   :: T.Text
, cinput  :: [Signal]
, coutput :: [Signal]
, cinout :: [Signal]
, creg :: [Always]
, cassign :: [Assign]
, cinstance :: [Instance]
, cinstanceConnect :: M.Map InstanceName [(Signal,Signal)]
} deriving (Show,Read,Eq)
