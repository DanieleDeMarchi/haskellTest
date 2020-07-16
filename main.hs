fizz :: Int -> String
fizz n | n `mod` 15 == 0  = "FizzBuzz"
       | n `mod` 3  == 0  = "Fizz"
       | n `mod` 5  == 0  = "Buzz"
       | otherwise = show n

main :: IO ()    -- This says that main is an IO action.
main = return () -- This tells main to do nothing. (Serve a non dare errori in compilazione su repl)

type Person = (Name,Address) 
type Name = String
data Address = None | Addr String deriving (Eq,Show)

indirizzo :: Person -> String
indirizzo p | snd p == None = "Nessun indirizzo"
            | otherwise = show (snd p)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

fibPair :: (Eq a, Num a) => a -> (a,a)
fibPair 1 = (0 , 1)
fibPair n = (snd pair, fst pair + snd pair)
            where pair = fibPair(n-1)

fibTail :: (Eq a, Num a) => a -> a
fibTail n = snd (fibPair n) 

_length :: [a] -> Integer
_length [] = 0
_length (_:xs) = 1 + _length(xs)


_map :: (a->b) -> [a] -> [b]
_map _ [] = []
_map f (x:xs) = f x : _map f xs

squares n = _map (\x -> x*x) [1..n]

potenzaArray :: (Num a, Eq a, Enum a) => a -> a -> [a]
potenzaArray n e = _map (\x -> (potenza x e)) [1..n]

potenza :: (Num a, Eq a) => a -> a -> a
potenza x 0 = 1
potenza x e = x * (potenza x (e-1))


add = \x ->( \y -> (x+y))

_succ = add 1

addList :: (Num a) => [a] -> [a] -> [a]
addList [] [] = []
addList [] ys = ys
addList xs [] = xs
addList (x:xs)(y:ys) = (x+y) : addList xs ys

fibStream :: (Num a) => [a]
fibStream = 1:1:(addList fibStream (tail fibStream))

takeEl 0 (x:_) = x
takeEl n (_:xs) = takeEl (n-1) xs


_mapComp f xs = [ f x | x <- xs] 

quickSortParam :: (Ord a) => [a] -> [a]
quickSortParam [] = []
quickSortParam (x:xs) = quickSortParam [y | y <- xs , y < x] ++ (x : quickSortParam [y | y <- xs , y > x])

--reverse inefficiente
_reverse [] = []
_reverse (x:xs) = _reverse(xs) ++ [x]


--reverse ricorsione di coda
reverseAcc [] ys = ys
reverseAcc (x:xs) ys = reverseAcc xs (x:ys)

_reverse2 xs = reverseAcc xs []

--reverse foldr

