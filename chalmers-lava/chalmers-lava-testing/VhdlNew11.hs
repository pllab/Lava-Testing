module VhdlNew11
  ( writeVhdlClk
  , writeVhdlNoClk
  , writeVhdlInputClk
  , writeVhdlInputNoClk
  , writeVhdlInputOutputClk
  , writeVhdlInputOutputNoClk
  )
 where

import Lava.Signal
import Lava.Netlist
import Lava.Generic
import Lava.Sequent
import Lava.Error
import Lava.LavaDir

import Data.List
  ( intersperse
  , nub
  )

import System.IO
  ( openFile
  , IOMode(..)
  , hPutStr
  , hClose
  , hSetBuffering
  , BufferMode (..)
  , stdout
  )

-- import System.IO
--   ( stdout
--   , BufferMode (..)
--   , hSetBuffering
--   )

import Data.IORef

--import IOBuffering
--  ( noBuffering
--  )

--import IOExts
--  ( IORef
--  , newIORef
--  , readIORef
--  , writeIORef
--  )

-- import System.Cmd (system)
-- import System.Exit (ExitCode(..))

--import System
--  ( system
--  , ExitCode(..)
--  )

----------------------------------------------------------------
-- write vhdl

writeVhdlClk :: (Constructive a, Generic b) => String -> (a -> b) -> IO ()
writeVhdlClk = writeVhdl True

writeVhdlNoClk :: (Constructive a, Generic b) => String -> (a -> b) -> IO ()
writeVhdlNoClk = writeVhdl False

writeVhdl :: (Constructive a, Generic b) => Bool -> String -> (a -> b) -> IO ()
writeVhdl clocked name circ =
  do writeVhdlInput clocked name circ (var "inp")

writeVhdlInputClk :: (Generic a, Generic b) => String -> (a -> b) -> a -> IO ()
writeVhdlInputClk = writeVhdlInput True

writeVhdlInputNoClk :: (Generic a, Generic b) => String -> (a -> b) -> a -> IO ()
writeVhdlInputNoClk = writeVhdlInput False

writeVhdlInput :: (Generic a, Generic b) => Bool -> String -> (a -> b) -> a -> IO ()
writeVhdlInput clocked name circ inp =
  do writeVhdlInputOutput clocked name circ inp (symbolize "outp" (circ inp))

writeVhdlInputOutputClk :: (Generic a, Generic b)
                     => String -> (a -> b) -> a -> b -> IO ()
writeVhdlInputOutputClk = writeVhdlInputOutput True

writeVhdlInputOutputNoClk :: (Generic a, Generic b)
                     => String -> (a -> b) -> a -> b -> IO ()
writeVhdlInputOutputNoClk = writeVhdlInputOutput False


writeVhdlInputOutput :: (Generic a, Generic b)
                     => Bool -> String -> (a -> b) -> a -> b -> IO ()
writeVhdlInputOutput clocked name circ inp out =
  do writeItAll clocked name inp (circ inp) out

writeItAll :: (Generic a, Generic b) => Bool -> String -> a -> b -> b -> IO ()
writeItAll clocked name inp out out' =
  do hSetBuffering stdout NoBuffering 
     putStr ("Writing to file \"" ++ file ++ "\" ... ")
     writeDefinitions clocked file name inp out out'
     putStrLn "Done."
 where
  file = name ++ ".vhd"
  
----------------------------------------------------------------
-- definitions

writeDefinitions :: (Generic a, Generic b)
                 => Bool -> FilePath -> String -> a -> b -> b -> IO ()
writeDefinitions clocked file name inp out out' =
  do firstHandle  <- openFile firstFile WriteMode
     secondHandle <- openFile secondFile WriteMode
     var <- newIORef 0
     

     hPutStr firstHandle $ unlines $
       [ "library ieee;"
       , ""
       , "use ieee.std_logic_1164.all;"
       , ""
       , "entity"
       , "  " ++ name
       , "is"
       , "port"
       , "  ( "
       , if clocked then "    clk : in std_logic ;" else " "
       , "    "] ++   -- , "  -- inputs"] ++
       [ "    " ++ v ++ " : in std_logic" | VarBool v <- [head inps]] ++
       [ "  ; " ++ v ++ " : in std_logic"
       | VarBool v <- tail inps
       ] ++
       [ ""
       , "  " -- outputs
       ] ++
       [ "  ; " ++ v ++ " : out std_logic"
       | VarBool v <- outs'
       ] ++
       [ "  );"
       , "end " ++ name ++ ";"
       , ""
       , "architecture"
       , "  structural"
       , "of"
       , "  " ++ name
       , "is"
       ]

       
     hPutStr secondHandle $ unlines $
       [ "begin"
       ]
     
     let new =
           do n <- readIORef var
              let n' = n+1; v = "w" ++ show n'
              writeIORef var n'
              hPutStr firstHandle ("  signal " ++ v ++ " : std_logic;\n")
              return v

         
         define v s =
           case s of
             Bool True     -> port "vdd"  []
             Bool False    -> port "gnd"  []
             Inv x         -> port "invG"  [x]

             And []        -> define v (Bool True)
             And [x]       -> port "wire"   [x]
             And [x,y]     -> port "andG" [x,y]
             And (x:xs)    -> define (w 0) (And xs)
                           >> define v (And [x,w 0])


             Or  []        -> define v (Bool False)
             Or  [x]       -> port "wire"   [x]
             Or  [x,y]     -> port "orG"  [x,y]
             Or  (x:xs)    -> define (w 0) (Or xs)
                           >> define v (Or [x,w 0])


             Xor  []       -> define v (Bool False)
             Xor  [x]      -> port "wire"   [x]
             Xor  [x,y]    -> port "xorG" [x,y]
             Xor  (x:xs)   -> define (w 0) (Or xs)
                           >> define (w 1) (Inv (w 0))
                           >> define (w 2) (And [x, w 1])
                           
                           >> define (w 3) (Inv x)
                           >> define (w 4) (Xor xs)
                           >> define (w 5) (And [w 3, w 4])
                           >> define v     (Or [w 2, w 5])

             VarBool s     -> port "wire" [s]
             DelayBool x y -> if clocked then port "delay" [x, y] else wrong Lava.Error.DelayEval
             
             _             -> wrong Lava.Error.NoArithmetic
           where
            w i = v ++ "_" ++ show i
            


            
            port "delay" [x, y] =
             do hPutStr secondHandle $
                    "  "
                   ++ make 9 ("c_" ++ v)
                   ++ " : entity work."
                   ++ make 5 "dff"
                   ++ " port map ("
                   ++ concat (intersperse ", " ("clk":[y] ++ [v]))
                   ++ ");\n"   


            

            port name args =
              do hPutStr secondHandle $
                      "  "
                   ++ make 9 ("c_" ++ v)
                   ++ " : entity work."
                   ++ make 5 name
                   ++ " port map ("
                   ++ concat (intersperse ", " (args ++ [v]))
                   ++ ");\n"    

     outvs <- netlistIO new define (struct out)
     hPutStr secondHandle $ unlines $
       [ ""
       , "  " -- , "  -- naming outputs"
       ]
     
     sequence
       [ define v' (VarBool v)
       | (v,v') <- flatten outvs `zip` [ v' | VarBool v' <- outs' ]
       ]
     
     hPutStr secondHandle $ unlines $
       [ "end structural;"
       ]
     
     hClose firstHandle
     hClose secondHandle
     
     --system ("cat " ++ firstFile ++ " " ++ secondFile ++ " > " ++ file)
     cat firstFile secondFile file
     --system ("rm " ++ firstFile ++ " " ++ secondFile)
     return ()
 where
  sigs x = map unsymbol . flatten . struct $ x
  
  inps  = sigs inp
  outs' = sigs out'
 
  firstFile  = file ++ "-1"
  secondFile = file ++ "-2"

  make n s = take (n `max` length s) (s ++ repeat ' ')

cat a b c = do
  do s <- readFile a
     writeFile c s
  do s <- readFile b
     appendFile c s

----------------------------------------------------------------
-- the end.

