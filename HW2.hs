-- Part 1: Recursive Functions

lconcat :: [[a]] -> [a]
lconcat l = 
  case l of (h:t) -> [h1] ++ t1 ++ lconcat t where (h1:t1) = h
            [] -> []

lfoldl :: ((a, b) -> b) -> b -> [a] -> b
lfoldl f e l = 
  case l of (h:t) -> lfoldl f (f (h, e)) t
            [] -> e


-- Part 2: Tail-Recursive Functions

fact :: Int -> Int
fact n =
  let
    factTR n nat | n > 1 = factTR (n-1) (nat*n)
                 | otherwise = nat
  in
    factTR n 1

power :: Int -> Int -> Int
power x n =
  let
    powerTR x n nat | n > 1 = powerTR x (n-1) (nat*x)
                    | n == 0 = 1
                    | otherwise = nat*x
  in
    powerTR x n 1

fib :: Int -> Int
fib n =
  let
    fibTR n pre cur | n == 0 = pre
                    | otherwise = fibTR (n-1) cur (pre+cur)
  in
    fibTR n 0 1 

lfilter :: (a -> Bool) -> [a] -> [a]
lfilter p l =
  let
    lfilterTR p l arr = 
      case l of (h:t)  -> if p h then lfilterTR p t (arr ++ [h])
                          else lfilterTR p t arr
                [] -> arr
  in
    lfilterTR p l []

ltabulate :: Int -> (Int -> a) -> [a]
ltabulate n f =
  let
    ltabulateTR n f arr | n > 0 = ltabulateTR (n-1) f ([f (n-1)] ++ arr)
                        | otherwise = arr
  in
    ltabulateTR n f []

union :: Eq a => [a] -> [a] -> [a]
union s t = 
  case t of (x:xs) -> if find s x then union s xs 
                      else union (s ++ [x]) xs
            [] -> s
  
find :: Eq a => [a] -> a -> Bool
find arr k = 
  case arr of (x:xs) -> if x == k then True
                       else find xs k
              [] -> False

data Tree t = Leaf t | Node (Tree t, t, Tree t)

inorder :: Tree a -> [a]
inorder t =
  let
    inorderTR t arr l = 
      case t of Node (t1,n,t2) -> inorderTR t1 arr ([t] ++ l)
                Leaf n -> 
                  case l of (Node (t1,k,t2):xs) -> inorderTR t2 (arr ++ [n] ++ [k]) xs
                            [] -> arr ++ [n]
  in
    inorderTR t [] []

postorder :: Tree a -> [a]
postorder t =
  let
    postorderTR t arr l = 
      case t of Node (t1,n,t2) -> postorderTR t2 ([n] ++ arr) ([t1] ++ l)
                Leaf n -> 
                  case l of (x:xs) -> postorderTR x ([n] ++ arr) xs
                            [] -> [n] ++ arr
  in
    postorderTR t [] []


preorder :: Tree a -> [a]
preorder t =
  let
    preorderTR t arr l = 
      case t of Node (t1,n,t2) -> preorderTR t1 (arr ++ [n]) ([t] ++ l)
                Leaf n -> 
                  case l of (Node (t1,k,t2):xs) -> preorderTR t2 (arr ++ [n]) xs
                            [] -> arr ++ [n]
  in
    preorderTR t [] []

-- Part 3: Sorting

quicksort :: (Ord a) => [a] -> [a]
quicksort l = 
  case l of (h:t) -> let small = quicksort [k | k <- t, k <= h]
                         big = quicksort [k | k <- t, k > h]
                     in small ++ [h] ++ big
            [] -> []

mergesort :: (Ord a) => [a] -> [a]
mergesort (h1:h2:t) = conquer  (mergesort front) (mergesort back)
                        where (front, back) = split (h1:h2:t)
mergesort (h:t) = [h]
mergesort [] = []
            
conquer :: Ord a => [a] -> [a] -> [a]
conquer l [] = l
conquer [] l = l
conquer (h0:t0) (h1:t1) | h0 <= h1 = [h0] ++ conquer t0 (h1:t1)
                        | otherwise = [h1] ++ conquer (h0:t0) t1
                      
split :: [a] -> ([a], [a])
split l =
  case l of (h0:h1:t) -> (h0:t0, h1:t1) where (t0, t1) = split t
            (h:t) -> ([h],[])
            [] -> ([],[])

-- Part 4: Heap

type Loc = Int -- to be defined by students
type Heap t = [(Loc,t)] -- to be defined by students


heapEmpty :: () -> Heap a
heapEmpty () = []

heapAllocate :: Heap a -> a -> (Heap a, Loc)
heapAllocate h v = (h ++ [(length h + 1,v)], length h + 1)

heapDereference :: Heap a -> Loc -> Maybe a
heapDereference h l =
  case h of ((pos,v):xs) -> if pos == l then Just v
                            else k where k = heapDereference xs l
            [] -> Nothing

heapUpdate :: Heap a -> Loc -> a -> Maybe (Heap a)
heapUpdate h l v = 
  case h of ((pos,k):xs) -> if pos == l then Just ([(pos,v)] ++ xs)
                            else Just ([(pos,k)] ++ t) where Just (t) = heapUpdate xs l v
            [] -> Nothing
-- DONE!

main = putStrLn "Load Complete!"
