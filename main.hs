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


