-- solve NYT Digits puzzle (David F. Place 2023)
import Data.List (delete, unfoldr)
import Data.Maybe
import System.Environment

data Ops = Add | Sub | Mul | Div deriving (Eq) 

instance Show Ops where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " ÷ "

type CompletedOperation = (Int, Ops, Int, [Int])
type Guess = (Int, [Int], [CompletedOperation])

ops = [(Div,div), (Add, (+)), (Sub, (-)), (Mul, (*))]
           
-- Don't make pairs where a < b
                    
pairs numbers = concatMap f numbers 
  where f a = catMaybes $ map (g a) $ ns
              where ns = delete a numbers
                    g a b
                      | (a < b) = Nothing
                      | otherwise = Just (a, b, delete b ns)

safeHead x
  | null x = Nothing
  | otherwise = Just $ head x

-- create a lazy list of the solutions in order by length using breadth-first search

digitsBF :: [Int] -> Int -> [Guess] 
digitsBF numbers target = -- breadth-first search finds the shortest solution
  (filter p) . concat . (unfoldr f) . genFirstLevel $ numbers
  where p (n,_,_) = target == n 
        f [] = Nothing
        f x = Just (x, genNextLevel x)
                                    
genFirstLevel numbers = map (\n -> (n, numbers, [])) numbers
 
genNextLevel level = concatMap f level
  where f (_, nums, completedOperations) = 
          concatMap g $ pairs nums   
            where g (a, b, others) = 
                    genGuesses a b others completedOperations

genGuesses a b others cO =   
   catMaybes [gDiv a b others cO,
               gOp a Mul b others cO,
               gOp a Add b others cO,
               gOp a Sub b others cO]
  where gOp a op b others completedOperations =
          Just (ans, others', (a, op, b, others'):completedOperations)
          where o = fromJust $ lookup op ops
                ans = a `o` b
                others' = ans:others
        gDiv a b others completedOperations 
          | b == 0 = Nothing -- don't divide by zero
          | rem a b /= 0 = Nothing -- only look into a non-fractional result
          | otherwise = Just (ans, others', (a, Div, b, others'):completedOperations) 
          where ans = a `div` b -- integer division
                others' = ans:others 
            
-- a depth first approach to find solutions that use all the numbers

digitsDF :: [Int] -> Int -> [CompletedOperation] -> Maybe [CompletedOperation]
digitsDF numbers target guesses
  | (length numbers) == 1 = if target == (head numbers) 
                                then Just $ reverse guesses 
                                else Nothing
  | otherwise = tryOperations numbers target guesses

tryOperations numbers target guesses = safeHead $ catMaybes $ map f $ pairs numbers
  where f (a,b,ns) = safeHead $ catMaybes
           [gOp a Add b ns,
            gOp a Sub b ns,
            gOp a Mul b ns,
            gDiv a b ns]
        gOp a op b ns = digitsDF ns' target ((a, op, b, ns'):guesses)
          where o = fromJust $ lookup op ops
                ans = a `o` b
                ns' = ans:ns   
        gDiv a b ns 
          | b == 0 = Nothing
          | rem a b /= 0 = Nothing
          | otherwise = digitsDF ns' target ((a, Div, b, ns'):guesses)
            where ns' = (a `div` b):ns    

-- output

doDigits numbers target = do printSolution $ digitsBF numbers target

doDeep numbers target = do
  let solution = digitsDF numbers target []
  if isNothing solution
    then putStrLn "No Solution."
    else mapM_ printOp $ fromJust solution
                          
printSolution solutions = do 
  let (_,_,completedOperations) = head solutions -- just the first one
  if null solutions
    then putStrLn "No Solution" -- Boundary case
    else if (null completedOperations)
            then putStrLn "Trivial Case" -- Boundary case
            else mapM_ printOp $ reverse completedOperations 
                       
showFormula a op b ans = output ++ (replicate (20 - (length output)) ' ')
  where output = (show a) ++ (show op) ++ (show b) ++ " = " ++ (show ans) 
         
printOp (a, op, b, ns) = do 
    putStr $ showFormula a op b $ (fromJust $ lookup op ops) a b
    putStrLn $ show ns

-- usage: ./digits b|d target numbers..

main = do args <- getArgs
          let algorithm = head args
              nums = tail args
              target = read $ head nums
              numbers = map read $ tail nums
          case algorithm of
            "d" -> doDeep numbers target
            "b" -> doDigits numbers target
            _   -> putStrLn "usage: ./digits b|d target numbers..."                     



          
  
  
