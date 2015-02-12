{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE OverloadedStrings#-}
import Prelude hiding (exp)
import Text.Shakespeare.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Trans.State
import qualified Data.Map as M

class Verilog a where
  toVerilog :: a -> T.Text

data Range =
    Range Int Int
  | Bit
  deriving (Show,Read,Eq)

data Signal = Signal {
  sname :: T.Text
, sbits :: Range
} deriving (Show,Read,Eq)

data Instance = Instance {
  iname :: T.Text
, icircuit :: Circuit  
, iconnect :: [(Signal,Signal)]
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

data Assign = Assign Signal Exp
  deriving (Show,Read,Eq)

data Exp = 
    If Exp Exp Exp
  | Mux Exp Exp Exp
  | Not Exp
  | Or Exp Exp
  | BitOr Exp Exp
  | And Exp Exp
  | BitAnd Exp Exp
  | Add Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Eq Exp Exp
  | S Signal
  | C Int
  | NonBlockAssign Exp Exp
  | BlockAssign Exp Exp
  deriving (Show,Read,Eq)

data Circuit = Circuit {
  cname   :: T.Text
, cinput  :: [Signal]
, coutput :: [Signal]
, cinout :: [Signal]
, cwire :: [Signal]
, creg :: [Always]
, cassign :: [Assign]
, cinstance :: [Instance]
} deriving (Show,Read,Eq)

initCircuit :: T.Text -> Circuit
initCircuit name = Circuit name [] [] [] [] [] [] []

type Cir = StateT Circuit IO

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
  let sig=Signal name bits
  put $ cir { cinput = sig:cinput cir}
  return sig

output :: Monad m => T.Text -> Range -> StateT Circuit m Signal
output name bits = do
  cir <- get
  let sig=Signal name bits
  put $ cir { coutput = sig:coutput cir}
  return sig

inout :: Monad m => T.Text -> Range -> StateT Circuit m Signal
inout name bits = do
  cir <- get
  let sig=Signal name bits
  put $ cir { cinout = sig:cinout cir}
  return sig

reg :: Monad m => T.Text -> Range -> [Stim] -> Exp -> StateT Circuit m (Signal)
reg name bits stim exp = do
  cir <- get
  let sig=Signal name bits
  put $ cir { creg = Always sig stim exp:creg cir}
  return sig

portList :: Circuit -> [Signal]
portList cir = cinput cir ++ coutput cir

portMap :: Circuit -> M.Map T.Text Signal
portMap cir = M.fromList $ map (\sig -> (sname sig,sig)) (portList cir)

inst :: Monad m => Circuit -> T.Text -> [(T.Text,Signal)] -> StateT Circuit m Instance
inst circ name conn = do
  cir <- get
  let m = portMap cir
  let inst'=Instance name circ $ map (\(n,s) ->(m M.! n,s)) conn
  put $ cir { cinstance = inst':cinstance cir}
  return inst'

instance Verilog Range where
  toVerilog (Range f t) = [st|[#{f}:#{t}]|]
  toVerilog Bit = ""

instance Verilog Exp where
  toVerilog (If a b c) = [st|
  if (#{toVerilog a}) begin
    #{toVerilog b}
  end else begin
    #{toVerilog c}
  end 
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
  toVerilog (S (Signal name range))= [st|#{name}#{toVerilog range}|]
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
  toVerilog (Always sig stims exp) = [st|//
  always @(#{stimstr}) begin
#{toVerilog nbexp}
  end
|]
    where
      stimstr = T.intercalate " or " $ map (\stim -> toVerilog stim) $ stims
      nbexp = nonblockExp' (S sig) exp

instance Verilog Assign where
  toVerilog (Assign sig exp) = [st|  assign #{toVerilog (S sig)} = #{toVerilog exp}|]

instance Verilog Instance where
  toVerilog (Instance name cir connects) = [st|  #{cname cir} #{name}(#{conn});|]
    where
      conn = T.intercalate ",\n" $ map (\((Signal fn frange),(Signal tn trange))-> [st|.#{fn}(#{tn}#{toVerilog trange})|]) $ connects

instance Verilog Circuit where
  toVerilog cir = [st|// 
module #{cname cir} (#{portlist});
#{iplist}#{oplist}#{inoutplist}
#{reglist}#{wirelist}
#{assignlist}
#{alwayslist}
#{instlist}
endmodule
|]
    where
      portlist   = T.intercalate ", " $ map sname $ portList cir
      iplist     = T.unlines $ map (\(Signal n range) -> [st|  input #{toVerilog range} #{n};|]) $ cinput cir
      oplist     = T.unlines $ map (\(Signal n range) -> [st|  output #{toVerilog range} #{n};|])$ coutput cir
      inoutplist = T.unlines $ map (\(Signal n range) -> [st|  inout #{toVerilog range} #{n};|]) $ cinout cir
      reglist    = T.unlines $ map (\(Signal n range) -> [st|  reg #{toVerilog range} #{n};|])   $ map alsig $ creg cir
      wirelist   = T.unlines $ map (\(Signal n range) -> [st|  wire #{toVerilog range} #{n};|])  $ cwire cir
      alwayslist = T.unlines $ map toVerilog $ creg cir
      assignlist = T.unlines $ map toVerilog $ cassign cir
      instlist = T.unlines $ map toVerilog $ cinstance cir

-- sim :: Circuit -> IO ([(Signal,Val)] -> [(Signal,Val)])
-- sim cir = error "not"
{-
1, clk-posedge
2, always
3, assign

-}
dff :: Circuit
dff = circuit "dff" $ do
  clk <- input "clk" Bit
  rstn <- input "rstn" Bit
  din <- input "din" Bit
  output "dout" Bit
  reg "dout" Bit [Posedge clk,Negedge rstn] $
    If (Not (S rstn))
      (C 0)
      (S din)

dff8 :: Circuit
dff8 = circuit "dff8" $ do
  clk <- input "clk" Bit
  rstn <- input "rstn" Bit
  din <- input "din" $ Range 7 0
  output "dout" $ Range 7 0
  reg "dout" (Range 7 0) [Posedge clk,Negedge rstn] $
    If (Not (S rstn))
      (C 0)
      (S din)

main :: IO ()
main = do
  cir <- circuitM "Top" $ do
    clk <- input "clk" Bit
    din <- input "din" Bit
    rstn <- input "rstn" Bit
    dout <- output "dout" Bit
    inst dff "dff" [("clk",clk),
                    ("din",din),
                    ("rstn",rstn),
                    ("dout",dout)]
  let verilog = toVerilog cir
  T.putStr $ verilog
  let verilog = toVerilog dff
  T.putStr $ verilog
  let verilog = toVerilog dff8
  T.putStr $ verilog
  return ()

