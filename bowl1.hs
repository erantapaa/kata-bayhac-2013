{-# LANGUAGE NoMonomorphismRestriction #-}

data Bowl = Pins Int Int | Spare Int Int | Strike | Incomplete Int
  deriving (Show)

frames' :: [Int] -> [ Bowl ]
frames' [] = []
frames' (10:xs) = Strike : frames' xs
frames' (a:b:xs) = 
  if a+b == 10
    then (Spare a b):frames' xs
    else (Pins a b):frames' xs
frames' [a] = [ Incomplete a ]

frames :: [Int] -> [Bowl]
frames = frames'

score' :: Int -> [Bowl] -> [Int]
score' _ [] = []
score' _ (Incomplete _:_)          = []
score' 11 _ = []
score' n (Strike:Strike:Strike:xs) = 30 : score' (n+1) (Strike:Strike:xs)
score' n (Strike:Spare a b:xs)     = 20 : score' (n+1) (Spare a b:xs)
score' n (Strike:Pins a b:xs)      = 10 + a + b : score' (n+1) (Pins a b:xs)
score' n (Strike:_)                = [] -- error "incomplete frame for strike (1)"
score' n (Spare a b: Strike:xs)    = 20 : score' (n+1) (Strike:xs)
score' n (Spare a b:Spare c d:xs)  = 10 + c : score' (n+1) (Spare c d:xs)
score' n (Spare a b:Pins c d:xs)   = 10 + c : score' (n+1) (Pins c d:xs)
score' n (Spare _ _:_)             = [] -- error "incomplete frame for spare"
score' n (Pins a b:xs)             = a + b : score' (n+1) xs

score :: [Bowl] -> [Int]
score bs = score' 1 bs

disp :: Bowl -> String
disp (Pins a b)     = (show a) ++ " " ++ (show b)
disp (Spare a b)    = (show a) ++ " /"
disp Strike         = "  X"
disp (Incomplete _) = "???"

zipfill :: [Bowl] -> [Int] -> [(Bowl,Maybe Int)]
zipfill [] _ = []
zipfill (b:bs) (n:ns) = (b,Just n) : zipfill bs ns 
zipfill (b:bs) []     = (b, Nothing ) : zipfill bs []

foo :: [Int] -> [(Bowl, Maybe Int)]
foo xs = zipfill bowls ns
  where bowls = frames xs
        ns = score bowls

disp2 :: (Bowl, Maybe Int) -> String
disp2 (b, Just n)  = disp b ++ " -- " ++ (show n)
disp2 (b, Nothing) = disp b ++ " -- "

foo2 :: [Int] -> IO ()
foo2 xs = putStrLn $ unlines $ map disp2 (foo xs)


rolls0 = replicate 12 10

rolls1 = replicate 20 1

rolls2 =[0,10,0,10,0,10,0,10,0,10,0,10,0,10,0,10]

rolls3 = [10,0,10,0,10,0,10,0,10,0,10,0,10,0,10,0,10,0,10]

rolls4 = replicate 20 3

rolls5 = [10,1,9,2,8,3,7,4,6]


test1 = []

test2 = [1,2,3,4]

test3 = [7,3,8,1]

test4 = [10,8,1]

test5 = [1,2,3]

test6 = [7,3]

test7 = [10]

test8 = [10,3]

