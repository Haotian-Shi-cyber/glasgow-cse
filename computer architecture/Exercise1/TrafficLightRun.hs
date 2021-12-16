module Main where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Register
import Trafficlight

main :: IO ()
main = do
  putStrLn "*** trafficlight1 simulation ***"
  trafficlight1Driver
  putStrLn "\n\n*** trafficlight2 simulation ***"
  trafficlight2Driver

---------------------------------------------------------------------------------------
-- simulate trafficlight1
---------------------------------------------------------------------------------------

trafficlight1_test_data :: [String]
trafficlight1_test_data = ["1" ,"0" ,"0", "0", "0", "0", "0","0","0","0","0","0","0","0","0","0","0","0","0","0"]

trafficlight1Driver :: IO ()
trafficlight1Driver = driver $ do

-- input data
    useData trafficlight1_test_data
-- input ports
    in_reset <- inPortBit "reset"
-- input signals
    let reset = inbsig in_reset
-- connect circuit to input and output signal
    let(green, amber, red) = trafficlight1 reset
-- Format the simulation output
    format 
        [string "reset = ", bit reset,
        string "   green,amber,red = ", bit green, bit amber, bit red]

    runSimulation 

-----------------------------------------------------------------------------
-- Simulate trafficlight2
----------------------------------------------------------------------------

trafficlight2_test_data :: [String]
trafficlight2_test_data =
--      reset  walkrequest        green amber red wait walk walkCount(16bit)
        ["0     0",             --0     0      0    0   0   0
         "0     0",      --(g/i)--1     0      0    1   0   0               ###normal conditions ###
         "0     1",      --(g/i)--1     0      0    1   0   1
         "0     0",      --(a/i)--0     1      0    1   0   1
         "0     0",      --(r/l)--0     0      1    0   1   1
         "0     0",      --(r/l)--0     0      1    0   1   1
         "0     0",      --(r/l)--0     0      1    0   1   1
         "0     0",      --(a/i)--0     1      0    1   0   1
         "0     0",      --(g/i)--1     0      0    1   0   1               ### normal conditions end ###
         "0     1",      --(g/i)--1     0      0    1   0   2               ### Boundary conditions ####
         "0     0",      --(a/i)--0     1      0    1   0   2
         "0     1",      --(r/l)--0     0      1    0   1   2
         "0     1",      --(r/l)--0     0      1    0   1   3               ## press walk request while r/walk
         "0     1",      --(r/l)--0     0      1    0   1   4
         "0     1",      --(a/i)--0     1      0    1   0   5               ## press walk request while a/waik
         "0     0",      --(g/i)--1     0      0    1   0   6               
         "0     0",      --(g/i)--1     0      0    1   0   6
         "1     0",      --(g/i)--1     0      0    1   0   6               ## press reset reset the walkcounter
         "0     0",      --(g/i)--1     0      0    1   0   0
         "0     1",      --(g/i)--1     0      0    1   0   0               ## press walk request to see if reset button influente the function
         "0     0",      --(a/i)--0     1      0    1   0   1
         "0     0",      --(r/l)--0     0      1    0   1   1
         "0     1",      --(r/l)--0     0      1    0   1   1
         "0     0",      --(r/l)--0     0      1    0   1   2 
         "0     0",      --(a/i)--0     1      0    1   0   2
         "0     0",      --(g/i)--1     0      0    1   0   2
         "1     0",      --(g/i)--1     0      0    1   0   2				## reset the counter
         "0     0",      --(g/i)--1     0      0    1   0   0
         "0     0",      --(g/i)--1     0      0    1   0   0
         "0     0",      --(g/i)--1     0      0    1   0   0
         "0     0"]      --(g/i)--1     0      0    1   0   0               

trafficlight2Driver :: IO ()
trafficlight2Driver = driver $ do

-- input data
    useData trafficlight2_test_data
-- input ports
    in_reset <- inPortBit "reset"
    in_walkrequest <- inPortBit "walkrequest"
-- input signals
    let reset = inbsig in_reset
    let walkrequest = inbsig in_walkrequest
-- connect circuit to input and output signal
    let(green, amber, red, wait, walk, walkCount) = trafficlight2 reset walkrequest
-- Format the simulation output
    format 
        [string "reset = ", bit reset,
        string "    walkrequest = ", bit walkrequest,
        string "    output: gar = ", bit green, bit amber, bit red,
        string "    wait/walk = ", bit wait, bit walk,
        string "    walkCount = ",bindec 16 walkCount]

    runSimulation 