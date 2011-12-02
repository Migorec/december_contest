
import Data.List

postfix2infix = head . foldl f [] 
   where 
     f :: [String] -> String -> [String]
     f (x:y:zs) "+"    = ("("++y ++"+"++ x++")"):zs
     f (x:y:zs) "-"    = ("("++y ++"-"++ x++")"):zs
     f (x:y:zs) "*"    = ("("++y ++"*"++ x++")"):zs
     f (x:y:zs) "/"    = ("("++y ++"/"++ x++")"):zs
     f (x:y:zs) "^"    = ("("++y ++"^"++ x++")"):zs
     f (x:zs)   "SQRT" = ("sqrt("++ x++")"):zs
     f (x:zs)   "ID"   = x:zs
     f (x:zs)   "ABS"  = ("|"++x++"|"):zs
     f xs       y      = y : xs

calc :: [String] -> Double
calc = head . foldl f [] 
   where 
     f :: [Double] -> String -> [Double]
     f (x:y:zs) "+"    = (y + x):zs
     f (x:y:zs) "-"    = (y - x):zs
     f (x:y:zs) "*"    = (y * x):zs
     f (x:y:zs) "/"    = (y / x):zs
     f (x:y:zs) "^"    = (y ** x):zs
     f (x:zs)   "SQRT" = (sqrt x):zs
     f (x:zs)   "ID"   = x:zs
     f (x:zs)   "ABS"  = (abs x):zs
     f xs       y      = read y : xs

oplist :: Int -> [[String]] -> [String] -> [[String]]     
oplist 0 a _  = a
oplist k a op = oplist (k-1) [x++[o] | o <- op, x<-a] op

solve :: [Double] -> [String] -> [String] ->Double->[String]
solve num binop unop r= map (postfix2infix) (filter (\x -> calc x == r) [mix e uo | e <- bexp, uo <-unoplist])
    where bopcount = length num - 1
          numlist = permutations num
          boplist = oplist bopcount [[]] binop
          bexp = [(map show n)++o |n <- numlist, o <- boplist]
          unopcount = bopcount + (length num)
          unoplist = oplist unopcount [[]] unop
          mix [] [] = []
          mix (x:xs) (y:ys) = x:y:(mix xs ys) 
            
