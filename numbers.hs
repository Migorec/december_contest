import Data.List
data Result = Result String Double deriving (Eq,Show)

data Op = Op String

eval :: Double -> Double -> Op -> Double
eval a b (Op "+") = a + b
eval a b (Op "-") = a - b
eval a b (Op "*") = a * b
eval a b (Op "/") = a / b
eval a b (Op "^") = a ** b

write :: Op -> String -> String ->  String
write (Op s) s1 s2 = "("++s1++s++s2++")"

useop ::[Result] -> [Op] -> [[Result]]
useop [res] _ =[[res]] 
useop resl ops = [ (Result (write op s1 s2 ) (eval d1 d2 op)):(resl\\[res1,res2]) |res1@(Result s1 d1) <- resl, res2@(Result s2 d2) <- (resl \\ [res1]),op<-ops ]

solve :: [Double] -> [Op] -> Double->[Result]
solve nums ops res= filter (\(Result _ d)  -> d==res) (map head (f [resnums]))
    where resnums = map (\x -> Result (show x) x) nums
          f :: [[Result]] -> [[Result]]
          f rl | length(head rl) == 1 = rl
               |otherwise =f$concat$map (\x -> useop x ops) rl
    