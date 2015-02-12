
module HsVerilog.Simulation where

import qualified Data.Text as T
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Map as M

import HsVerilog.Type

val' :: Exp -> Reader Circuit Integer
val' (If a b c) = do
  v <- val' a
  if v /= 0 then val' b else val' c
val' (Mux a b c) = val' $ If a b c
val' (Not a) = do
  v <- val' a
  return $ if v == 0 then 1 else 0
val' (Or a b) = do
  a' <- val' a
  b' <- val' b
  case (a',b') of
    (0,0) -> return 0
    (0,_) -> return 1
    (_,0) -> return 1
    (_,_) -> return 1
val' (BitOr a b) = do
  a' <- val' a
  b' <- val' b
  case (a',b') of
    (0,0) -> return 0
    (0,_) -> return 1
    (_,0) -> return 1
    (_,_) -> return 1
val' (And a b) = do
  a' <- val' a
  b' <- val' b
  case (a',b') of
    (0,0) -> return 0
    (0,_) -> return 0
    (_,0) -> return 0
    (_,_) -> return 1
val' (BitAnd a b) = do
  a' <- val' a
  b' <- val' b
  case (a',b') of
    (0,0) -> return 0
    (0,_) -> return 0
    (_,0) -> return 0
    (_,_) -> return 1
val' (Add a b) = do
  a' <- val' a
  b' <- val' b
  return $ a' + b'
val' (Sub a b) = do
  a' <- val' a
  b' <- val' b
  return $ a' - b'
val' (Mul a b) = do
  a' <- val' a
  b' <- val' b
  return $ a' * b'
val' (Div a b) = do
  a' <- val' a
  b' <- val' b
  return $ a' `div` b'
val' (Eq a b) = do
  a' <- val' a
  b' <- val' b
  return $ if a' == b' then 1 else 0
val' (S a) = do
  cir <- ask
  return $ sval $ (sym cir (sname a))
val' (C a) = return $ a
val' (NonBlockAssign _ _) = error "do not eval this"
val' (BlockAssign _ _) = error "do not eval this"

sym' :: Circuit -> M.Map T.Text Signal
sym' cir = M.fromList $ map (\sig -> (sname sig,sig)) $ concat $ map (\f -> f cir) [cinput,(map alsig).creg,(map assig).cassign]
sym :: Circuit -> T.Text -> Signal
sym cir name = sym' cir M.! name
  
val :: Circuit -> Exp -> Integer
val cir exp' = flip runReader cir $ val' exp'

readReg :: Monad m => T.Text ->  StateT Circuit m Integer
readReg name = do
  cir <- get
  let m = M.fromList $ map (\sig -> (sname sig,sig)) $ concat $ map (\f -> f cir) [(map alsig).creg]
  return $ sval $ m M.! name

readInput :: Monad m => T.Text ->  StateT Circuit m Integer
readInput name = do
  cir <- get
  let m = M.fromList $ map (\sig -> (sname sig,sig)) $ concat $ map (\f -> f cir) [cinput]
  return $ sval $ m M.! name

readOutput :: Monad m => T.Text ->  StateT Circuit m Integer
readOutput name = do
  cir <- get
  let m = M.fromList $ map (\sig -> (sname sig,sig)) $ concat $ map (\f -> f cir) [coutput]
  return $ sval $ m M.! name

readAssign :: Monad m => T.Text ->  StateT Circuit m Integer
readAssign name = do
  cir <- get
  let m = M.fromList $ map (\sig -> (sname sig,sig)) $ concat $ map (\f -> f cir) [(map assig).cassign]
  return $ sval $ m M.! name

(<==) :: Monad m => T.Text -> Integer -> StateT Circuit m ()
(<==) name v = do
  cir <- get
  let inps = map (update v) $ cinput cir
  put $ cir {cinput=inps}
  where
    update v' sig@(Signal name' _range _) | name' == name = sig{sval=v'}
                                          | otherwise = sig

simM :: Monad m => Circuit -> StateT Circuit m a -> m Circuit
simM circuit act = do
  (_,cir) <- flip runStateT circuit act
  return cir

print' :: MonadIO m => StateT Circuit m ()
print' = do
  cir <- get
  liftIO $ print cir

updateReg :: Monad m => StateT Circuit m ()
updateReg = do
  cir <- get
  regs <- forM (creg cir) $ \r -> do
    return $ r {alsig = (alsig r) {sval = val cir (alexp r)}}
  put $ cir { creg = regs }

-- updateInput :: Circuit -> Circuit
-- updateInput cir = flip runReader cir $ do
--   regs <- forM (creg cir) $ \r -> do
--     return $ r {alsig = (alsig r) {sval = val cir (alexp r)}}
--   return $ cir { creg = regs }

-- updateOutput :: Circuit -> Circuit
-- updateOutput cir = flip runReader cir $ do
--   regs <- forM (creg cir) $ \r -> do
--     return $ r {alsig = (alsig r) {sval = val cir (alexp r)}}
--   return $ cir { creg = regs }

-- updateAssign :: Circuit -> Circuit
-- updateAssign cir = flip runReader cir $ do
--   regs <- forM (creg cir) $ \r -> do
--     return $ r {alsig = (alsig r) {sval = val cir (alexp r)}}
--   return $ cir { creg = regs }

-- updateWire :: Circuit -> Circuit
-- updateWire cir = flip runReader cir $ do
--   regs <- forM (creg cir) $ \r -> do
--     return $ r {alsig = (alsig r) {sval = val cir (alexp r)}}
--   return $ cir { creg = regs }
