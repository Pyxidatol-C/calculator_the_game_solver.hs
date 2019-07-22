type State = ((Int, Maybe Int), [Op])

data Op = Add Int | Sub Int | Mul Int | Div Int | Insert Int |
    Sum | Reverse | Replace Int Int | Delete | Shift | Inv10 |
    Neg | Mirror | StoreNew | StoreInsert | IncrementMeta Int

instance Show Op where
    show op = case op of
        Add    n        -> "+ " ++ show n
        Sub    n        -> "- " ++ show n
        Mul    n        -> "* " ++ show n
        Div    n        -> "/ " ++ show n
        Insert n        -> show n
        Sum             -> "SUM"
        Reverse         -> "Reverse"
        Replace a b     -> show a ++ " => " ++ show b
        Delete          -> "<<"
        Shift           -> "Shift >"
        Inv10           -> "Inv10"
        Neg             -> "+/-"
        Mirror          -> "Mirror"
        StoreNew        -> "Store"
        StoreInsert     -> "Store"
        IncrementMeta n -> "[+] " ++ show n

toDigits :: Int -> [Int]
toDigits 0 = [0]
toDigits n | n < 0     = error "WTF this shouldn't happen"
           | otherwise = toDigits' n
  where
    toDigits' 0 = []
    toDigits' m = (m `mod` 10) : toDigits' (m `div` 10)

toInt :: [Int] -> Int
toInt = foldr (\x y -> x + 10 * y) 0

replaceDigits :: [Int] -> [Int] -> [Int] -> [Int]
replaceDigits ds old new = sub ds [] [] old
  where
    sub bs res _   [] = sub bs (res ++ new) [] old
    sub [] res buf _  = res ++ buf
    sub (b : bs) res buf (c : cs)
        | b == c    = sub bs res (buf ++ [b]) cs
        | otherwise = sub bs (res ++ buf ++ [b]) [] old

replaceInt :: Int -> Int -> Int -> Int
replaceInt n a b = toInt $ reverse (replaceDigits n' a' b')
  where
    n' = reverse $ toDigits n
    a' = reverse $ toDigits a
    b' = reverse $ toDigits b

increment :: Int -> Op -> Op
increment n (Add    m   ) = Add (m + n)
increment n (Sub    m   ) = Sub (m + n)
increment n (Mul    m   ) = Mul (m + n)
increment n (Div    m   ) = Div (m + n)
increment n (Insert m   ) = Insert (m + n)
increment n (Replace a b) = Replace (a + n) (b + n)
increment _ op            = op

execute :: Op -> State -> [State]
execute (Add m) ((n, mStore), ops) = [((n + m, mStore), ops)]
execute (Sub m) ((n, mStore), ops) = [((n - m, mStore), ops)]
execute (Mul m) ((n, mStore), ops) = [((n * m, mStore), ops)]
execute (Div m) ((n, mStore), ops)
    | n `mod` m == 0 = [((n `div` m, mStore), ops)]
    | otherwise      = []
execute (Insert m) ((n, mStore), ops) =
    [((toInt (toDigits m ++ toDigits n), mStore), ops)]
execute Sum ((n, mStore), ops) = [((sum $ toDigits n, mStore), ops)]
execute Reverse ((n, mStore), ops) =
    [(((toInt . reverse . toDigits) n, mStore), ops)]
execute (Replace a b) ((n, mStore), ops) = [((replaceInt n a b, mStore), ops)]
execute Delete ((n, mStore), ops) =
    [(((toInt . tail . toDigits) n, mStore), ops)]
execute Shift ((n, mStore), ops) =
    let digits = toDigits n
    in  [((toInt (tail digits ++ [head digits]), mStore), ops)]
execute Inv10 ((n, mStore), ops) = [((toInt (map f digits), mStore), ops)]
  where
    digits = toDigits n
    f x = (10 - x) `mod` 10
execute Neg ((n, mStore), ops) = [((-n, mStore), ops)]
execute Mirror ((n, mStore), ops) =
    let ds = toDigits n in [((toInt (reverse ds ++ ds), mStore), ops)]
execute StoreNew    ((n, _     ), ops) = [((n, Just n), ops)]
execute StoreInsert ((n, mStore), ops) = case mStore of
    Nothing -> []
    Just m  -> execute (Insert m) ((n, mStore), ops)
execute (IncrementMeta m) ((n, mStore), ops) =
    [((n, mStore), fmap (increment m) ops)]

eval :: (Int -> Int) -> [(State, [Op])] -> [(State, [Op])]
eval f statesWithHistory = do
    ((st, ops), history) <- statesWithHistory
    op                   <- ops
    ((n, store), ops')   <- execute op (st, ops)
    return (((f n, store), ops'), op : history)

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
    let steps = iterate (eval f) [(((v0, Nothing), ops), [])]
    in  take (nbSteps + 1) steps

main :: IO ()
main =
    let v0      = 0
        goal    = -13
        nbMoves = 4
        ops     = [Add 3, Sub 7, Neg]
        f       = id
        -- Change the parameters above according to the lvl
        res     = solution v0 goal nbMoves ops f
    in  print res
