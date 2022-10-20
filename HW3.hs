-- 'TVar' as the type for variables
type TVar = String

-- 'Var' for variables
-- 'Abs' for λ-abstractions
-- 'App' for λ-applications
data Expr = Var TVar
          | Abs (TVar, Expr)
          | App (Expr, Expr) deriving (Show)

--------------------------------------------------------------------------------

-- FV(e)
getFreeVariables :: Expr -> [TVar]
getFreeVariables e = 
  let 
    freeVariables e arr = 
      case e of Var t -> if search arr t then delete arr t
                         else arr ++ [t]
                Abs (t, ep) -> freeVariables ep (arr ++ [t])
                App (ep1,ep2) -> freeVariables ep1 l where l = freeVariables ep2 arr
  in
    freeVariables e []
      
search :: Eq a => [a] -> a -> Bool
search [] k = False
search (h:t) k | h == k = True
               | otherwise = search t k 
               
delete :: Eq a => [a] -> a -> [a]
delete [] k = []
delete (h:t) k | h == k = t
               | otherwise = [h] ++ delete t k

-- Substitution [e'/x]e
substitute :: (Expr, TVar) -> Expr -> Expr
substitute (e', x) e = 
  case e of Var k -> if k == x then e'
                     else e
            Abs (k,ep) -> if k == x then Abs (k, ep)
                          else if search (getFreeVariables e') k then Abs (getFreshVariable x e e',substitute (e',x) e) 
                          else Abs (k, substitute (e',x) e)
            App (ep1,ep2) -> App (substitute (e',x) ep1,substitute (e',x) ep2)
 
-- Call-by-Name
stepCallByName :: Expr -> Expr
stepCallByName expr = 
  case expr of Var _ -> expr
               Abs _ -> expr
               App (ep1,ep2) -> 
                case ep1 of Var _ -> App (ep1,stepCallByName ep2)
                            Abs (k,eps) -> substitute (ep2,k) eps
                            App _ -> App(stepCallByName ep1, ep2)

-- Call-by-Value
stepCallByValue :: Expr -> Expr
stepCallByValue expr = 
  case expr of Var _ -> expr  
               Abs _ -> expr
               App (ep1,ep2) ->
                case ep1 of Var _ -> App (ep1, stepCallByValue ep2)
                            App _ -> App (stepCallByValue ep1,ep2)
                            Abs (k,eps) ->
                              case ep2 of Var _ -> substitute (ep2, k) eps
                                          Abs _ -> substitute (ep2, k) eps
                                          App _ -> App (ep1,stepCallByValue ep2)


--Fresh Variables
getFreshVariable :: TVar -> Expr -> Expr -> TVar
getFreshVariable varName e e' =
  let
    union :: [TVar] -> [TVar] -> [TVar]
    union [] l2 = l2
    union (h1:t1) l2 =
      if elem h1 l2 then
        union t1 l2
      else
        union t1 (l2 ++ [h1])

    getAllNames :: Expr -> [TVar]
    getAllNames (Var varName) = [varName]
    getAllNames (Abs (varName, e')) = union [varName] (getAllNames e')
    getAllNames (App (e1, e2)) = union (getAllNames e1) (getAllNames e2)

    isFreshVar :: TVar -> Bool
    isFreshVar [] = True
    isFreshVar (h:t) = (h == '\'') && isFreshVar t

    findLongestFreshVar :: TVar -> [TVar] -> TVar -> TVar
    findLongestFreshVar targetVarName [] longestFreshVar = longestFreshVar
    findLongestFreshVar targetVarName (h:t) longestFreshVar =
      let
        prefix  = take (length targetVarName) h
        postfix = drop (length targetVarName) h
      in
        if (prefix == targetVarName
            && isFreshVar postfix
            && length h > length longestFreshVar) then
          findLongestFreshVar targetVarName t h
        else
          findLongestFreshVar targetVarName t longestFreshVar
    
    allNames = union (getAllNames e) (getAllNames e')
  in
    (findLongestFreshVar varName allNames varName) ++ "'"
--------------------------------------------------------------------------------

main = putStrLn "Load Complete!"
