import           CalculatorTheG

main :: IO ()
main =
    let v0      = 25
        goal    = 822
        nbMoves = 6
        ops     = [Mirror, Ins "5", StoreNew, StoreInsert, Delete]
        f       = portal 3 1
    in  putStrLn $ showSolution v0 goal nbMoves ops f
