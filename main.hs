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

--reverse inefficiente
_reverse [] = []
_reverse (x:xs) = _reverse(xs) ++ [x]


--reverse ricorsione di coda
reverseAcc [] ys = ys
reverseAcc (x:xs) ys = reverseAcc xs (x:ys)

_reverse2 xs = reverseAcc xs []

--reverse foldl
reverseFoldl xs = foldl (\ xs x -> x:xs ) [] xs

--reverse foldr
reverseFoldr xs = foldr (\y ys -> (ys ++ [y])) [] xs

data Tree a = Leaf a | Branch (Tree a) (Tree a)

height :: Tree a -> Integer
height = \t -> case t of
                Leaf n -> 0
                Branch t1 t2 -> max (1 + (height t1)) (1 + (height t2))

treeProva = Branch (Branch (Branch (Leaf 5) (Leaf 6)) (Leaf 6)) (Leaf 6)


--sommaPari pattern matching

--sommaPari ricorsione di coda
sommaPariAux acc []  = acc
sommaPariAux acc (x:xs) | x `mod` 2 == 0 = sommaPariAux (acc+x) xs 
                        | True = sommaPariAux acc xs 
                      
sommaPari = sommaPariAux 0

--sommaPari foldl foldr
sommaPariFoldl xs = foldl (+) 0 [x | x <- xs , x `mod` 2 == 0]
sommaPariFoldr xs = foldr (+) 0 [x | x <- xs , x `mod` 2 == 0]
sommaPariFoldl2 xs = foldl (\ n m -> if m `mod` 2 == 0 then n+m else n) 0 xs
sommaPariFoldr2 xs = foldr (\ m n -> if m `mod` 2 == 0 then n+m else n) 0 xs

--alberi binari
data BTree a = Null | BTree a (BTree a) (BTree a)

btreeProva = BTree 0 (BTree 5 (BTree 7 Null Null)(BTree 8 Null Null)) (BTree 5 Null (BTree 5 Null Null))

--profondità albero binario
depthBtree Null = 0
depthBtree (BTree n tl tr) = 1 + (max (depthBtree tl) (depthBtree tr))
--somma albero binario
sommaBtree Null = 0
sommaBtree (BTree n tl tr) = sommaBtree tl + sommaBtree tr + n


-------------------------------------------------------
-----------------  Esercizi Esami   -------------------
-------------------------------------------------------

 ---Scrivere una funzione Haskell che controlli se due liste sono l’una una 
 -- permutazione dell’altra. L’unica operazione permessa sugli elementi della 
 -- lista `e il test di uguaglianza. Si definisca inoltre il tipo Haskell 
 -- della funzione definita e delle funzioni ausiliarie.

checkPermutation [][] = True
checkPermutation _ [] = False
checkPermutation [] _ = False
checkPermutation (x:xs) ys = case (remove x ys) of
                              Nothing -> False
                              Just yss -> checkPermutation xs yss
remove x [] = Nothing
remove x (y:ys) | x == y = Just ys
                | True = concMaybe y (remove x ys)

concMaybe y Nothing = Nothing
concMaybe y (Just ys) = Just (y : ys)  


-- Occorrenze in albero binario
occorrenze x Null = 0
occorrenze x (BTree y tl tr) = (if x == y then 1 else 0) + occorrenze x tl + occorrenze x tr

{- 
Il grafo di una funzione parziale f : A → B `e definito come l’insieme di coppie 
{(a, b) | b = f(a)}, ossia l’insieme di coppie in cui i primi elementi costituiscono 
il dominio della funzione e i secondi elementi definiscono il comportamento della 
funzione, sui corrispondenti primi elementi.
Scrivere una funzione Haskell che preso il grafo di una funzione f da A in B, 
rappresentato come lista di coppie, ed una lista l di elementi di tipo A, 
applica la funzione f a tutti gli elementi della lista.
Si tenga presente che la funzione f pu`o essere parziale (non definita su alcuni elementi), 
nel caso la lista l contenga un elemento su cui f non `e definita, l’elemento viene rimosso dalla lista.
Scrivere inoltre una funzione Haskell che, dato un lista di coppie, controlli che la lista non contenga due coppie con il primo elemento uguale.
-}

--partialFunction :: (Num a, Eq a) => [(a , a)] -> a -> Maybe a
partialFunction [] x = Nothing
partialFunction ((v,r):fl) x | v==x = Just r 
                             | True = partialFunction fl x

funzione = [(1,2), (3,4), (5,6)]

partialFunctionListAux fl li = map (partialFunction fl) li

_filter [] = []
_filter (Nothing : xs) = _filter xs
_filter (Just x : xs) = x : _filter xs 

partialFunctionList fl l = _filter (partialFunctionListAux fl l)



{----------------------------------
      Elmininazione prima riga e prima colonna da matrice 
      quadrata rappresentata come lista di liste
-----------------------------------}

eliminaRigaColonna [] = []
eliminaRigaColonna (x:xs) = eliminaColonna xs

eliminaColonna [] = []
eliminaColonna (x:xs) = (tail x) : (eliminaColonna xs)

matriceProva = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

diagonaleMatrice [] = []
diagonaleMatrice (x:xs) = (head x) : diagonaleMatrice (eliminaRigaColonna (x:xs))


{----------------------------------
        Trasposta matrice
-----------------------------------}

trasposta ([]:_) = []
trasposta xs = map head xs : trasposta ( map tail xs )


{----------------------------------
  Coppia massimo minimo in lista
-----------------------------------}
coppiaMaxMin :: (Num a, Ord a) => [a] -> Maybe (a, a)
coppiaMaxMin [] = Nothing
coppiaMaxMin (x:xs) = Just (_max, _min) where
                  _max = massimoList xs x
                  _min = minimoList xs x

massimoList [] temp = temp
massimoList (x:xs) temp | x >= temp = massimoList xs x
                        | x < temp = massimoList xs temp
minimoList [] temp = temp
minimoList (x:xs) temp | x >= temp = minimoList xs temp
                       | x < temp = minimoList xs x

testList = [1,3..21] ++ [20,19..0]

_filter2 [] = []
_filter2 (Nothing : xs) = _filter2 xs
_filter2 (Just x : xs) = x : _filter2 xs 

matriceMaxMin [] = []
matriceMaxMin xs = _filter2 (map coppiaMaxMin xs)



-------------------------------------------------------
--------------  Esercizi Numeri Liste -----------------
-------------------------------------------------------

binomial n k = (fact n) `div` ( (fact k) * (fact (n - k) ) )

--Scrivere una funzione che data una lista ne costruisce una rimuovendo gli elementi di posizione pari (si conti partendo da 1).

removePariAux [] acc = []
removePariAux (x:xs) acc | (acc + 1) `mod` 2 == 0 = removePariAux xs (acc+1)
                         | (acc + 1) `mod` 2 /= 0 = x : removePariAux xs (acc+1)

removePari xs = removePariAux xs 0

--2. Scrivere una funzione che calcola la somma degli elementi di posizione dispari di una lista.

sommaPosizioniDispari xs = foldl (+) 0 xs 

--3. Scrivere il QuickSort (polimorfo).
quickSortParam :: (Ord a) => [a] -> [a]
quickSortParam [] = []
quickSortParam (x:xs) = quickSortParam [y | y <- xs , y < x] ++ (x : quickSortParam [y | y <- xs , y > x])


--4. Scrivere una funzione che calcola i 2 minori elementi dispari di una lista (se esistono). Ad esempio minOdd([2,3,4,6,8,7,5]) riduce a (3,5)

coppiaMinimo [] = Nothing
coppiaMinimo (x:xs) | length xs == 0 = Nothing
                    | True = Just (min1, min2) where
                              min1 = minimoList xs x
                              min2 = minimoList (removeList xs (minimoList xs x)) x

removeList [] _ = []
removeList (x:xs) n | x == n = removeList xs n
                    | True = x : (removeList xs n)


{-
5. Scrivere una funzione che costruisce, a partire da una lista di numeri interi, una lista di coppie in cui
(a) il primo elemento di ogni coppia `e uguale all’elemento di corrispondente posizione nella lista originale e
(b) il secondo elemento di ogni coppia `e uguale alla somma di tutti gli elementi conseguenti della lista originale.
-}

sommaSeguenti [] = []
sommaSeguenti (x:xs) = (x, (foldl (+) 0 xs)) : (sommaSeguenti xs)


--6. Somma precedenti in coppia
sommaPrecedentiAux [] acc = []
sommaPrecedentiAux (x:xs) acc = (x, acc) : sommaPrecedentiAux xs (acc + x)

sommaPrecedenti [] = []
sommaPrecedenti xs = sommaPrecedentiAux xs 0

{-
7. Si scriva una funzione Haskell shiftToZero che data una lista costruisce un nuova lista che contiene
gli elementi diminuiti del valore minimo.
-}

shiftToZero [] = []
shiftToZero xs = map (\x -> x - (minimum xs)) xs

-------------------------------------------------------
----------------  Esercizi Matrici  -------------------
-------------------------------------------------------

{-
1. Si scriva una funzione matrix_dim che data una matrice ne calcola le dimensioni, se la matrice `e
ben formata, altrimenti restituisce (-1,-1).
-}

matrixDim [] = (0, 0)
matrixDim (x:xs) | (checkBenFormata (length x) xs) == True = dimMatriceOk (x:xs)
                 | True = (-1, -1)

checkBenFormata n [] = True
checkBenFormata n (x:xs) | (length x) == n = checkBenFormata n xs 
                         | True = False

countCols :: (Eq a, Num a) => [[a]] -> a
countCols [] = 0
countCols (x:xs) = 1 + countCols xs

dimMatriceOk (x:xs) = ((length x), (countCols (x:xs)))

-- 2. Si scriva una funzione colsums che data una matrice calcola il vettore delle somme delle colonne.
sommaColonne :: (Eq a, Num a) => [[a]] -> [a]
sommaColonne ([]:_) = []
sommaColonne xs = foldl (+) 0 (map head xs) : sommaColonne ( map tail xs)

-- 3.Si scriva una funzione colaltsums che, data una matrice implementata come liste
--   di liste perrighe, calcola il vettore delle somme a segni alternati delle colonne della matrice

alternaSegniMatrice [] = []
alternaSegniMatrice xs = alternaSegniMatriceAux xs 0

alternaSegniMatriceAux [] i = []
alternaSegniMatriceAux (x:xs) i = (alternaSegniRiga x i 0) : alternaSegniMatriceAux (x:xs) (i+1)

alternaSegniRiga [] i j = []
alternaSegniRiga (x:xs) i j | ((i+j) `mod` 2) == 0 = x : alternaSegniRiga xs i (j+1)
                            | True = -x : alternaSegniRiga xs i (j+1)
