
type Frame = Int

score' :: Frame -> [Int] -> Int
--        frame -> scores -> total
score' 10 xs = sum xs
score' n (10:a:b:xs) = 10 + a + b + score' (n+1) (a:b:xs)
score' n (a:b:c:xs)  =
  if a + b == 10
    then 10 + c + score' (n+1) (c:xs)
    else a + b + score' (n+1) (c:xs)

score :: [Int] -> Int
score xs = score' 1 xs

-- 300
test1 = score [10,10,10,10,10,10,10,10,10,10,10,10]

-- 20
test2 = score $ replicate 20 1

-- 2
test3 = score' 10 [1,1]

-- 4
test4 = score' 9 [1,1,1,1]

-- 14
test5 = score' 10 [3,7,4]

-- 20
test6 = score' 1 $ replicate 20 1

-- 100
test7 = score' 1 [0,10,0,10,0,10,0,10,0,10,0,10,0,10,0,10,0,10,0,10]

