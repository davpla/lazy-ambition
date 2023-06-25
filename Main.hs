-- solve NYT Digits puzzle (David F. Place 2023)
import Data.List (delete, unfoldr)
import Data.Maybe

-- create a lazy list of the solutions in order by length using breadth-first search

type CompletedOperation = (Int, String, Int, [Int])
type Guess = (Int, [Int], [CompletedOperation])
digitsBF :: [Int] -> Int -> [Guess] 
digitsBF numbers target = -- breadth-first search finds the shortest solution
  filter p $ concat $ unfoldr f $ genFirstLevel numbers
  where p (n,ns,_) = target == n 
        f [] = Nothing
        f x = Just (x, genNextLevel x)
                                    
genFirstLevel numbers = map (\n -> (n, numbers, [])) numbers
 
genNextLevel level = concatMap f level
  where f (_, nums, completedOperations) = 
          concatMap g $ pairs nums   
            where g (a, b, others) = 
                    genGuesses a b others completedOperations
                                        
pairs numbers = concatMap f numbers 
  where f a = map (g ns a) $ ns
              where ns = delete a numbers
                    g numbers a b = (a, b, delete b numbers)
                                                            
-- the rules of the game

genGuesses a b others cO = catMaybes $ 
  [gDiv a b others cO,gMul a b others cO,gAdd a b others cO,gSub a b others cO]
  where gMul a b others completedOperations
          | a < b = Nothing -- only one form for a commutative operation
          | otherwise = Just (ans, others', (a, " * ", b, others'):completedOperations)
          where ans = a * b
                others' = ans:others
        gAdd a b others completedOperations
          | a < b = Nothing -- only one form for a commutative operation
          | otherwise = Just (ans, others', (a, " + ", b, others'):completedOperations)
          where ans = a + b
                others' = ans:others
        gDiv a b others completedOperations 
          | b == 0 = Nothing -- don't divide by zero
          | rem a b /= 0 = Nothing -- only look into the non-fractional result
          | otherwise = Just (ans, others', (a, " ÷ ", b, others'):completedOperations) 
          where ans = a `div` b -- integer division
                others' = ans:others 
        gSub a b others completedOperations 
          | ans <= 0 = Nothing -- only look into the positive combination
          | otherwise = Just (ans, others', (a, " - ", b, others'):completedOperations)
          where ans = a - b
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
          (if (a < b) then [] else [aAdd a b ns, aSub a b ns, aMul a b ns, aDiv a b ns])
        aAdd a b ns = digitsDF ns' target ((a, " + ", b, ns'):guesses)  
            where ns' = (a + b):ns                           
        aSub a b ns = digitsDF ns' target ((a, " - ", b, ns'):guesses)
            where ns' = (a - b):ns        
        aMul a b ns = digitsDF ns' target ((a, " * ", b, ns'):guesses)
            where ns' = (a * b):ns         
        aDiv a b ns 
          | b == 0 = Nothing
          | rem a b /= 0 = Nothing
          | otherwise = digitsDF ns' target ((a, " ÷ ", b, ns'):guesses)
            where ns' = (a `div` b):ns    
                 
safeHead x
  | null x = Nothing
  | otherwise = Just $ head x  

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
                         
ops :: [(String,Int->Int->Int)]           
ops = [(" + ", (+)), (" - ", (-)), (" * ", (*)), (" ÷ ",div)]  

showFormula a op b ans = output ++ (replicate (20 - (length output)) ' ')
  where output = (show a) ++ op ++ (show b) ++ " = " ++ (show ans) 
         
printOp (a, op, b, ns) = do 
    putStr $ showFormula a op b $ (fromJust $ lookup op ops) a b
    putStrLn $ show ns

-- usage: ./Main algorithm target numbers...

main = do args <- getArgs
          let algorithm = head args
              nums = tail args
              target = (read $ head nums)::Int
              numbers = (map read $ tail nums)::[Int]
          if algorithm == "d"
            then doDeep numbers target
            else doDigits numbers target
                                  
  
