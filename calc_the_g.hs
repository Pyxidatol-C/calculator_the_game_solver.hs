import           Data.Char                      ( isNumber )

type State = ((Int, Maybe Int), [Op])

data Op = Add Int | Sub Int | Mul Int | Div Int | Ins String | Neg |
    Sum | Reverse | Replace String String | Delete | Shift | Inv10 |
    Mirror | StoreNew | StoreInsert | IncrementFuckingBloodyMeta Int
    deriving (Eq)

instance Show Op where
    show op = case op of
        Add n                        -> "+" ++ show n
        Sub n                        -> "-" ++ show n
        Mul n                        -> "*" ++ show n
        Div n                        -> "/" ++ show n
        Ins n                        -> n
        Sum                          -> "SUM"
        Reverse                      -> "Reverse"
        Replace a b                  -> a ++ " => " ++ b
        Delete                       -> "<<"
        Shift                        -> "Shift >"
        Inv10                        -> "Inv10"
        Neg                          -> "+/-"
        Mirror                       -> "Mirror"
        StoreNew                     -> "Store (new)"
        StoreInsert                  -> "Store (Ins)"
        IncrementFuckingBloodyMeta n -> "[+]" ++ show n

toDigits :: Int -> [Int]
toDigits 0 = [0]
toDigits n
    | n < 0 = error "Internal: attempted to geet digits of a negative number?!"
    | otherwise = toDigits' n
  where
    toDigits' 0 = []
    toDigits' m = (m `mod` 10) : toDigits' (m `div` 10)

stringToDigits :: String -> [Int]
stringToDigits s = reverse $ map ((read :: String -> Int) . pure) s

digitsToString :: [Int] -> String
digitsToString = map (head . show)

toInt :: [Int] -> Int
toInt = foldr (\x y -> x + 10 * y) 0

replaceDigits :: [Int] -> [Int] -> [Int] -> [Int]
replaceDigits ds old new = sub ds [] [] old
  where
    sub bs res _   [] = sub bs (res ++ new) [] old
    sub [] res buf _  = res ++ buf
    sub (b : bs) res buf (c : cs)
        | b == c        = sub bs res (buf ++ [b]) cs
        | b == head old = sub bs (res ++ buf) [b] (tail old)
        | otherwise     = sub bs (res ++ buf ++ [b]) [] old

replaceInt :: Int -> [Int] -> [Int] -> Int
replaceInt n as bs = toInt $ reverse (replaceDigits n' as' bs')
  where
    n'  = reverse $ toDigits n
    as' = reverse as
    bs' = reverse bs

increment :: Int -> Op -> Op
increment n (Add m) = Add (m + n)
increment n (Sub m) = Sub (m + n)
increment n (Mul m) = Mul (m + n)
increment n (Div m) = Div (m + n)
increment n (Ins m) = Ins
    (digitsToString . normaliseDigits $ addDigits (stringToDigits m)
                                                  (toDigits n)
    )
increment _ op = op

addDigits :: [Int] -> [Int] -> [Int]
addDigits xs ys | length xs < length ys = addDigits ys xs
                | otherwise             = zipWith (+) xs (ys ++ repeat 0)

normaliseDigits :: [Int] -> [Int]
normaliseDigits ds = normaliseDigits' ds 0 []

normaliseDigits' :: [Int] -> Int -> [Int] -> [Int]
normaliseDigits' [] 0 acc = acc
normaliseDigits' [] c acc = normaliseDigits' [c] 0 acc
normaliseDigits' (d : ds) c acc =
    let s = d + c in normaliseDigits' ds (s `div` 10) (s `mod` 10 : acc)


executeNeg :: Op -> State -> [State]
executeNeg op ((n, mStore), ops) =
    execute op ((-n, mStore), ops) >>= execute Neg

execute :: Op -> State -> [State]
execute (Add m) ((n, mStore), ops) = [((n + m, mStore), ops)]
execute (Sub m) ((n, mStore), ops) = [((n - m, mStore), ops)]
execute (Mul m) ((n, mStore), ops) = [((n * m, mStore), ops)]
execute (Div m) ((n, mStore), ops)
    | n `mod` m == 0 = [((n `div` m, mStore), ops)]
    | otherwise      = []

execute op@(Ins m) st@((n, mStore), ops)
    | not $ all isNumber m = error "You can only `Ins`ert a positive integer!"
    | n < 0 = executeNeg op st
    | otherwise = [((toInt (stringToDigits m ++ toDigits n), mStore), ops)]

execute op@Sum st@((n, mStore), ops)
    | n < 0     = executeNeg op st
    | otherwise = [((sum $ toDigits n, mStore), ops)]

execute op@Reverse st@((n, mStore), ops)
    | n < 0     = executeNeg op st
    | otherwise = [(((toInt . reverse . toDigits) n, mStore), ops)]

execute op@(Replace a b) st@((n, mStore), ops)
    | not $ all isNumber a = error "You can only replace a positive integer!"
    | not $ all isNumber b = error "You can only replace by a positive integer!"
    | n < 0                = executeNeg op st
    | otherwise            = [((replaceInt n as bs, mStore), ops)]
  where
    as = stringToDigits a
    bs = stringToDigits b

execute op@Delete st@((n, mStore), ops)
    | n < 0     = executeNeg op st
    | otherwise = [(((toInt . tail . toDigits) n, mStore), ops)]

execute op@Shift st@((n, mStore), ops)
    | n < 0
    = executeNeg op st
    | otherwise
    = let digits = toDigits n
      in  [((toInt (tail digits ++ [head digits]), mStore), ops)]

execute op@Inv10 st@((n, mStore), ops)
    | n < 0     = executeNeg op st
    | otherwise = [((toInt (map f digits), mStore), ops)]
  where
    digits = toDigits n
    f x = (10 - x) `mod` 10

execute Neg ((n, mStore), ops) = [((-n, mStore), ops)]

execute op@Mirror st@((n, mStore), ops)
    | n < 0
    = executeNeg op st
    | otherwise
    = let ds = toDigits n in [((toInt (reverse ds ++ ds), mStore), ops)]

execute StoreNew    ((n, _     ), ops) = [((n, Just n), ops)]

execute StoreInsert ((n, mStore), ops) = case mStore of
    Nothing -> []
    Just m  -> execute (Ins $ show m) ((n, mStore), ops)

execute (IncrementFuckingBloodyMeta m) ((n, mStore), ops) =
    [((n, mStore), fmap (increment m) ops)]

eval :: (Int -> Int) -> [((State, [Op]), [State])] -> [((State, [Op]), [State])]
eval f statesWithHistory = do
    (((st, ops), history), explored) <- statesWithHistory
    op                               <- ops
    ((n, store), ops')               <- execute op (st, ops)
    let st' = ((f n, store), ops')
    if st' `elem` explored
        then []
        else return ((st', op : history), st' : explored)

portal :: Int -> Int -> Int -> Int -- count i, j from right
portal i j n | i < j          = error "Must pipe to lower digit"
             | n < 0          = error "WTF n should be positive"
             | length ds <= i = n
             | otherwise      = portal i j portalSum
  where
    ds = toDigits n
    dropAt k xs = let (ys, zs) = splitAt k xs in ys ++ drop 1 zs
    padZeroes n' x = replicate n' 0 ++ [x]
    portalSum =
        let x = toInt $ dropAt i ds
            y = toInt $ padZeroes j (ds !! i)
        in  x + y

isAnswer :: Int -> State -> Bool
isAnswer goal ((n, _), _) = n == goal

solution :: Int -> Int -> Int -> [Op] -> (Int -> Int) -> [Op]
solution v0 goal nbSteps ops f = reverse moves
  where
    resList   = solution' v0 goal nbSteps ops f
    solutions = concatMap (filter (isAnswer goal . fst)) resList
    moves     = snd $ head solutions

solution' :: Int -> Int -> Int -> [Op] -> (Int -> Int) -> [[(State, [Op])]]
solution' v0 _ nbSteps ops f =
    let steps = iterate (eval f) [((((v0, Nothing), ops), []), [])]
    in  fmap fst <$> take (nbSteps + 1) steps

main :: IO ()
main =
    let v0      = 25
        goal    = 822
        nbMoves = 6
        ops     = [Mirror, Ins "5", StoreNew, StoreInsert, Delete]
        f       = portal 3 1
        -- Change the parameters above according to the lvl
        res     = solution v0 goal nbMoves ops f
    in  print res
