module Processor where

import qualified Lava
import qualified Lava.Arithmetic as LA
import qualified Lava.Patterns as LP
import qualified Lava.SequentialCircuits as LS
import qualified Lava.Operators as Op

data Op
    = And
    | Or
    | Add
    | Sub
    | VarOp String -- for construction
    deriving (Eq, Show) -- Can't use Enum because of non-nullary constructor

iToOp 0 = And
iToOp 1 = Or
iToOp 2 = Add
iToOp 3 = Sub
iToOp _ = error "only 4 ops"

instance Lava.Generic Op where
    struct o = Lava.Object $ Lava.unSignal $ Lava.varInt (show o)
    construct (Lava.Object s) =
        case Lava.unsymbol s of
            Lava.Int i -> iToOp i
            _ -> error "should't be constructing Op enum from non-int"

instance Lava.Constructive Op where
    zero = iToOp 0
    var = VarOp
    random rnd = iToOp $ Lava.valRnd rnd

-- sel1 sel0 out
--   0    0    a
--   0    1    b
--   1    0    c
--   1    1    d
mux4 ([sel0, sel1], (a, b, c, d)) = out
    where out0 = Lava.mux (sel1, (a, c))
          out1 = Lava.mux (sel1, (b, d))
          out  = Lava.mux (sel0, (out0, out1))

-- This is almost like row connection pattern, right?
forEach f ([], []) = []
forEach f (a:as, b:bs) = c:cs
    where
        c = f (a, b)
        cs = forEach f (as, bs)

alu :: Int -> (Lava.Signal Int, Lava.Signal Int, Op) -> Lava.Signal Int
alu n (a, b, And) = LA.bin2int $ forEach Op.and2 (LA.int2bin n a, LA.int2bin n b)
alu n (a, b, Or)  = LA.bin2int $ forEach Op.or2 (LA.int2bin n a, LA.int2bin n b)
alu _ (a, b, Add) = Op.plus (a, b)
alu _ (a, b, Sub) = Op.sub (a, b)
alu _ (a, b, _) = Lava.int 0

-- This needs to take in a carry in and output a carry out to work, unlike the
-- above one that works with ints directly
aluBitWise :: (Lava.Signal Bool, Lava.Signal Bool, Op) -> Lava.Signal Bool
aluBitWise (a, b, And) = Op.and2 (a, b)
aluBitWise (a, b, Or)  = Op.or2 (a, b)
aluBitWise (a, b, Add) = Op.int2bit $ Op.plus (Op.bit2int a, Op.bit2int b)
aluBitWise (a, b, Sub) = Op.int2bit $ Op.sub (Op.bit2int a, Op.bit2int b)
aluBitWise (a, b, _) = Lava.low

-- aluBitWise (a, b, Add) = Op.int2bit $ LA.fullAdd (cin, (a, b))
-- aluBitWise (a, b, Sub) = Op.int2bit $ Op.sub (Op.bit2int a, Op.bit2int b) -- do a + (~b + 1) == a - b

alu' n (a, b, op) = forEach f (a', b')
    where 
        a' = LA.int2bin n a
        b' = LA.int2bin n b
        f (a, b) = aluBitWise (a, b, op)

-- aluLL (cin, a, b, op) = out
--     where (addOut, cOut) = LA.adder a b
--           (negP1, _) = LA.adder (l)
--           (subOut, cOut) = LA.adder a (LA.add)