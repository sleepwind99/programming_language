-- example
addTwoInts :: Int -> Int -> Int
addTwoInts x y = x + y

sum_ :: Int -> Int
sum_ n | n == 1 = 1 
       | otherwise = n + sum_ (n-1) 

fac :: Int -> Int
fac n | n == 1 = 1 
      | otherwise = n * fac (n-1)

fib :: Int -> Int
fib n | n == 0 || n == 1 = 1 
      | otherwise = fib (n-1) + fib (n-2)

gcd_ :: Int -> Int -> Int
gcd_ m n | (m `mod` n) == 0 = n 
         | otherwise = gcd_ n (m `mod` n)

max_ :: [Int] -> Int
max_ l =
  case l of (h:t) -> if max_ t > h then max_ t else h
            [] -> 0

data Tree t = Leaf t | Node (Tree t, t, Tree t)

sum_tree :: Tree Int -> Int
sum_tree t = 
  case t of Node (t1,t0,t2) -> t0 + sum_tree t1 + sum_tree t2
            Leaf t -> t

depth :: Tree a -> Int
depth t =
  case t of Node (t1,t0,t2) -> 1 + depth t1 + depth t2
            Leaf t -> 0

bin_search :: Tree Int -> Int -> Bool
bin_search t x = 
  case t of Node (t1,t0,t2) -> if x == t0 then True else if x < t0 then bin_search t1 x else bin_search t2 x
            Leaf t -> if x == t then True else False

preorder :: Tree a -> [a]
preorder t = 
  case t of Node (t1,t0,t2) -> [t0] ++ preorder t1 ++ preorder t2
            Leaf t -> [t]

list_add :: [Int] -> [Int] -> [Int]
list_add l1 l2 = 
  case l1 of (h0:t0) -> case l2 of (h1:t1) -> [h0+h1] ++ list_add t0 t1
                                   [] -> [h0] ++ list_add t0 []
             [] -> case l2 of (h1:t1) -> [h1] ++ list_add [] t1
                              [] -> []

insert :: Int -> [Int] -> [Int]
insert m l = 
  case l of (h:t) -> if m > h then [h] ++ insert m t else [m] ++ l
            [] -> l ++ [m]

insort :: [Int] -> [Int]
insort l = 
  case l of (h:t) -> insert h (insort t)
            [] -> []

compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \x -> g (f x)

curry_ :: ((a, b) -> c) -> (a -> b -> c)
curry_ f = \x y -> f (x, y)

uncurry_ :: (a -> b -> c) -> ((a, b) -> c)
uncurry_ f = \(x,y) -> f x y

multifun :: (a -> a) -> Int -> (a -> a)
multifun f n = \x -> if n > 0 then multifun f (n-1) (f x) else x

ltake :: [a] -> Int -> [a]
ltake l n = 
  case l of (h:t) -> if n > 0 then [h] ++ ltake t (n-1) else []
            [] -> []

lall :: (a -> Bool) -> [a] -> Bool
lall f l = 
  case l of (h:t) -> if f h then lall f t else False
            [] -> True

lmap :: (a -> b) -> [a] -> [b]
lmap f l = 
  case l of (h:t) -> [f h] ++ lmap f t
            [] -> []

lrev :: [a] -> [a]
lrev l = 
  case l of (h:t) -> lrev t ++ [h]
            [] -> []

lzip :: [a] -> [b] -> [(a, b)]
lzip x y = 
  case x of (h1:t1) -> case y of (h2:t2) -> [(h1,h2)] ++ lzip t1 t2
                                 [] -> []
            [] -> []

split :: [a] -> ([a], [a])
split l =
  case l of (h0:h1:t) -> ([h0] ++ t0,[h1] ++ t1) where (t0,t1) = split t
            (h:t) -> ([h],[])
            [] -> ([],[])

cartprod :: [a] -> [b] -> [(a, b)]
cartprod s t = [(x,y) | x <- s, y <- t]

main = putStrLn "Load Complete!"
