module Main where

import qualified Lava
import qualified Lava.Arithmetic as LA
import qualified Lava.Patterns as LP
import qualified VhdlNew11 as VHD

(|>) x f = f x

prop_HalfAddOutputNeverBothTrue (a, b) = ok
  where
    (sum, carry) = halfAdd (a, b)
    ok           = Lava.nand2 (sum, carry)

prop_FullAddCommutative (c, (a, b)) = ok
  where
    out1 = fullAdd (c, (a, b))
    out2 = fullAdd (c, (b, a))
    ok = out1 Lava.<==> out2

prop_AdderCommutative (as, bs) = ok
  where
    out1 = adder2 (as, bs)
    out2 = adder2 (bs, as)
    ok = out1 Lava.<==> out2
  
prop_AdderCommutative_ForSize n =
  Lava.forAll (Lava.list n) $ \as ->
    Lava.forAll (Lava.list n) $ \bs ->
      prop_AdderCommutative (as, bs)

-- binary adder which does not take in carry bit and throws away carry out
adder2 (as, bs) = cs
  where
    (cs, carryOut) = LA.adder (Lava.low, (as, bs))

prop_Equivalent circ1 circ2 a = ok
  where
    out1 = circ1 a
    out2 = circ2 a
    ok = out1 Lava.<==> out2

halfAdd (a, b) = (sum, carry)
  where
    sum = Lava.xor2 (a,b)
    carry = Lava.and2 (a,b)
  
fullAdd (cin, (a, b)) = (sum, cout)
  where
    (sum1, carry1) = halfAdd (a, b)
    (sum, carry2)  = halfAdd (cin, sum1)
    cout           = Lava.xor2 (carry2, carry1)

adder :: (Lava.Signal Bool, ([Lava.Signal Bool], [Lava.Signal Bool])) -> ([Lava.Signal Bool], Lava.Signal Bool)
adder (cin, ([], [])) = ([], cin)
adder (cin, (a:as, b:bs)) = (sum:sums, cout)
  where
    (sum, carry) = fullAdd (cin, (a, b))
    (sums, cout) = adder (carry, (as, bs))

adder' :: (Lava.Signal Bool, [(Lava.Signal Bool, Lava.Signal Bool)]) -> ([Lava.Signal Bool], Lava.Signal Bool)
adder' = LP.row fullAdd
  
add n (a, b) = out
  where
    as = LA.int2bin n a
    bs = LA.int2bin n b
    (ss, c) = adder (Lava.low, (as, bs))
    out = LA.bin2int (ss ++ [c])

main :: IO ()
main = do
  test1
 -- test2
 -- test3

-- Combinational examples
-- 1) Half adder
test1 = do
  --Lava.simulateSeq halfAdd Lava.domain |> mapM_ print
  Lava.writeVhdl "halfAdd" halfAdd
  VHD.writeVhdlNoClk "halfAddwithoutClk" halfAdd
  --Lava.satzoo prop_HalfAddOutputNeverBothTrue >>= print
  --Lava.minisat prop_HalfAddOutputNeverBothTrue >>= print
  --Lava.minisat prop_FullAddCommutative >>= print
  --Lava.minisat (prop_AdderCommutative_ForSize 2) >>= print
  Lava.minisat (prop_AdderCommutative_ForSize 32) >>= print
  --Lava.smv (prop_Equivalent (LA.fullAdd) fullAdd) >>= print
  
-- 2) Full adder
test2 = do
  Lava.simulate (add 16) (4, 5) |> print
  Lava.writeVhdlInputOutput "4BitAdder" adder
    (Lava.var "cin", (Lava.varList 4 "a", Lava.varList 4 "b"))
    (Lava.varList 4 "sum", Lava.var "cout")

-- 3) Full adder (via row)
test3 = do
  Lava.writeVhdlInputOutput "4BitAdder_row" adder'
    (Lava.var "cin", map (\i -> (Lava.var $ "a_" ++ show i, Lava.var $ "b_" ++ show i)) [0..4])
    (Lava.varList 4 "sum", Lava.var "cout")
