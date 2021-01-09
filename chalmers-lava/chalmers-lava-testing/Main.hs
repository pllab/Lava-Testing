module Main where

import qualified Lava

(|>) x f = f x

main :: IO ()
main =
  test1

test1 = do
  Lava.simulateSeq halfAdd Lava.domain |> mapM_ print
  Lava.writeVhdl "halfAdd" halfAdd
  Lava.satzoo prop_HalfAddOutputNeverBothTrue >>= print

prop_HalfAddOutputNeverBothTrue (a, b) = ok
  where
    (sum, carry) = halfAdd (a, b)
    ok           = Lava.nand2 (sum, carry)

halfAdd (a, b) = (sum, carry)
  where
    sum = Lava.xor2 (a,b)
    carry = Lava.and2 (a,b)
