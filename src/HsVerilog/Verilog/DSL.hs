{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE OverloadedStrings#-}

module HsVerilog.Verilog.DSL (
  signal
, initCircuit
, circuit
, circuitM
, input
, output
, inout
, reg
, reg'
, assign
, wire
, inst
, connect
, (.:)
, (><)
) where

import HsVerilog.Type
import Prelude hiding (exp)
--import Text.Shakespeare.Text
import qualified Data.Text as T
--import qualified Data.Text.IO as T
import Control.Monad.Trans.State
--import Control.Monad.Trans.Reader
--import Control.Monad
import Data.Monoid
import qualified Data.Map as M


signal :: T.Text -> Range -> Signal
signal name bit = Signal name bit 0

initCircuit :: T.Text -> Circuit
initCircuit name = Circuit name [] [] [] [] [] [] M.empty

circuit :: T.Text -> State Circuit a -> Circuit
circuit name act = flip evalState (initCircuit name) $ do
  _ <- act
  get

circuitM :: Monad m => T.Text -> StateT Circuit m a -> m Circuit
circuitM name act = do
  (_,cir) <- flip runStateT (initCircuit name) act
  return cir


input :: Monad m => T.Text -> Range -> StateT Circuit m Signal
input name bits = do
  cir <- get
  let sig=signal name bits
  put $ cir { cinput = cinput cir ++ [sig]}
  return sig

output :: Monad m => T.Text -> Range -> StateT Circuit m Signal
output name bits = do
  cir <- get
  let sig=signal name bits
  put $ cir { coutput = coutput cir ++ [sig]}
  return sig

inout :: Monad m => T.Text -> Range -> StateT Circuit m Signal
inout name bits = do
  cir <- get
  let sig=signal name bits
  put $ cir { cinout = cinout cir ++ [sig]}
  return sig

reg :: Monad m => T.Text -> Range -> [Stim] -> (Exp -> Exp) -> StateT Circuit m (Signal)
reg name bits stim exp = do
  cir <- get
  let sig=signal name bits
  put $ cir { creg = creg cir ++ [Always sig stim (exp (S sig))]}
  return sig

reg' :: Monad m => T.Text -> Range -> [Stim] -> Exp -> StateT Circuit m (Signal)
reg' name bits stim exp = do
  cir <- get
  let sig=signal name bits
  put $ cir { creg = creg cir ++ [Always sig stim exp]}
  return sig

assign :: Monad m => Signal -> Exp -> StateT Circuit m Signal
assign sig exp = do
  cir <- get
  put $ cir { cassign = cassign cir ++ [Assign sig exp]}
  return sig


wire :: Instance -> T.Text -> Signal
wire inst' name =
  let sig = (portMap (icircuit inst')) M.! name
  in sig {sname = iname inst' <> "_" <> sname sig}

inst :: Monad m => Circuit -> T.Text -> [(T.Text,Signal)] -> StateT Circuit m Instance
inst circ name conn = do
  cir <- get
  let m = portMap cir
  let inst'=Instance name circ
  put $ cir { cinstance = cinstance cir ++ [inst']}
  let signals=(map (\(n,s) ->(m M.! n,s)) conn) <> instOutputPort name circ
  cir' <- get
  put $ cir' { cinstanceConnect = cinstanceConnect cir' <> M.singleton name signals}
  return $ inst'

connect :: Monad m => Instance -> T.Text -> Signal -> StateT Circuit m ()
connect inst' instPort otherPort = do
  cir <- get
  let m = portMap (icircuit inst')
  let sig= m M.! instPort
  put $ cir { cinstanceConnect =  M.update (\v -> Just (v ++ [(sig,otherPort)])) (iname inst') (cinstanceConnect cir) }
  return ()

(.:) :: Instance -> T.Text -> Signal
(.:) = wire
infix 8 .:

(><):: Integer -> Integer -> Range
(><) = Range
infix 8 ><

portMap :: Circuit -> M.Map T.Text Signal
portMap cir = M.fromList $ map (\sig -> (sname sig,sig)) (portList cir)
portList :: Circuit -> [Signal]
portList cir = cinput cir ++ coutput cir

instOutputPort :: T.Text -> Circuit -> [(Signal,Signal)]
instOutputPort name cir =
  let op  = coutput cir
      ren v = (v,v{sname=name <> "_" <> sname v})
  in map ren op
