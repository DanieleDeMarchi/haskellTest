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
alternaSegniMatriceAux (x:xs) i = (alternaSegniRiga x i 0) : alternaSegniMatriceAux xs (i+1)

alternaSegniRiga [] i j = []
alternaSegniRiga (x:xs) i j | ((i+j) `mod` 2) == 0 = x : alternaSegniRiga xs i (j+1)
                            | True = -x : alternaSegniRiga xs i (j+1)

sommaColonneSegniAlternati xs = sommaColonne (alternaSegniMatrice xs)

{-
4. Si scriva una funzione colMinMax che, data una matrice implementata come liste di liste per righe,
calcola il vettore delle coppie (minimo,massimo) delle colonne della matrice.
-}

maxMinRow [] = []
maxMinRow xs = map (\x -> (minimum x, maximum x)) xs

--5. Matrice triangolare inferiore
checkTriangolareInferioreAux [] i = True
checkTriangolareInferioreAux (x:xs) i = if (checkZeroAfter x i 0) == True 
                                          then checkTriangolareInferioreAux xs (i+1)
                                          else False

checkZeroAfter [] i acc = True
checkZeroAfter (x:xs) i acc | acc <= i = checkZeroAfter xs i (acc+1)
                            | True = if x == 0 then checkZeroAfter xs i (acc+1)
                                               else False

checkTriangolareInferiore xs = checkTriangolareInferioreAux xs 1

triangolareInferiore = [[5,0,0,0],[1,4,0,0],[5,8,2,0],[4,1,9,3]]

--6. Matrice triangolare superiore
checkTriangolareSuperiore xs = checkTriangolareInferioreAux (trasposta xs) 1

--7. Si scriva un predicato diagonal che determina se una matrice (quadrata) `e diagonale
-- basta fare && tra lowerTriangular e upperTriangular

{-
8. Una matrice quadrata M di ordine n si dice convergente con raggio r se il modulo della somma
degli elementi di ogni riga, escluso quello sulla diagonale, `e inferiore a r.
Si scriva un predicato convergent m r che determina se una matrice (quadrata) m `e convergente
con raggio r
-}

sumRowExclude [] partialSum i escludi = partialSum
sumRowExclude (x:xs) partialSum i escludi | i == escludi = sumRowExclude xs partialSum (i+1) escludi
                                          | True = sumRowExclude xs (abs x+partialSum) (i+1) escludi

checkRaggioRiga xs raggio escludi = raggio > (sumRowExclude xs 0 0 escludi)

checkRaggioMatriceAux [] raggio riga = True
checkRaggioMatriceAux (x:xs) raggio riga = (checkRaggioRiga x raggio riga) && (checkRaggioMatriceAux xs raggio (riga+1))

checkRaggioMatrice xs raggio = checkRaggioMatriceAux xs raggio 0

-------Versione foldl map
checkRaggioMatriceFoldl :: [[Integer]] -> Integer -> Bool
checkRaggioMatriceFoldl xs raggio = foldl (&&) True (map (\x -> checkRaggioRiga (fst x) raggio (snd x) ) (zip xs [0..((length xs)-1)]) )


{-
11. Si scriva una funzione che data una matrice di dimensioni n×k ed una k ×m restituisca la matrice
prodotto corrispondente (di dimensioni n × m). Si assuma di moltiplicare matrici con dimensioni
compatibili e (se facesse comodo) matrici non degeneri.
-}

prodottoMatrici mx1 mx2 = map (\riga -> map (sommaProdottoLista riga) (trasposta mx2) ) mx1
                            where 
                                sommaProdottoLista _  [] = 0
                                sommaProdottoLista [] _  = 0
                                sommaProdottoLista (x:xs) (y:ys) = (x*y) + (sommaProdottoLista xs ys)


-------------------------------------------------------
------------------  Esercizi BST ----------------------
-------------------------------------------------------

data BST a = Void | Node {val :: a, l :: BST a , r :: BST a}
 deriving (Eq, Ord, Read, Show)

-- Esercizio 1 : somma i valori di un albero
sumBST :: (Ord a, Num a) => BST a -> a
sumBST Void = 0
sumBST (Node val l r) = val + (sumBST l) + (sumBST r)

-- Esercizio 3 valuta se le somme di tutti gli alberi di una lista sono uguali
checkSommaAlberi ::(Ord a, Num a) => [BST a] -> Bool
checkSommaAlberi []     = True
checkSommaAlberi (albero:alberi) = all (== (sumBST albero)) (map (\x -> sumBST x) alberi)

-- Esercizio 5 inserimento in BST
insertBST :: Ord a => BST a -> a -> BST a
insertBST Void x = Node x Void Void
insertBST (Node val l r) x | x == val = Node val l r
                           | x < val = Node val (insertBST l x) r
                           | True = Node val l (insertBST r x)

createBSTaux bst [] = bst
createBSTaux bst (x:xs) = createBSTaux (insertBST bst x) xs

createBST xs = createBSTaux Void xs

-- Esercizio 6. Si scriva una funzione bst2List che calcola la lista ordinata degli elementi di un BST.

inorder Void = []
inorder (Node val l r) = (inorder l) ++ [val] ++ (inorder r)

-- Esercizio 7. Si scriva una (semplice) funzione di ordinamento di liste come combinazione di funzioni fatte nei precedenti esercizi.

ordinaConBST xs = inorder (createBST xs)

-- Esercizio 8. Si scriva una funzione filtertree p t che costruisce una lista (ordinata) di tutti gli elementi dell'albero t che soddisfano il predicato p.
filtertree :: Ord a => (a -> Bool) -> BST a -> [a]
filtertree _ Void = []
filtertree p (Node val l r) = (filtertree p l) ++ filtredElement ++ (filtertree p r) 
                                  where 
                                    filtredElement | p val == True = [val]
                                                   | True = []

-- Esercizio 9 
{- Si scriva una funzione annotate che costruisca un nuovo BST che in ogni nodo contenga, al posto del valore originale, 
una coppia composta dal medesimo valore e dall’altezza del nodo stesso (la lunghezza del massimo cammino, cioè 1 + max(height(sx),height(dx))
-}

annotate :: Ord a => BST a -> BST (a,Int)
annotate Void = Void
annotate bst = case bst of
                        Node x Void Void -> Node (x,0) Void Void
                        Node x l    Void -> Node (x, 1 + (treeheight l)) (annotate l) Void
                        Node x Void r -> Node (x, 1 + (treeheight r)) Void (annotate r)
                        Node x l    r -> Node (x, 1 + (max (treeheight l) (treeheight r)) ) (annotate l) (annotate r)


{- 10. Si scriva un predicato (funzione a valori booleani) almostBalanced per determinare se un albero
binario ha la seguente proprietà: per ogni nodo le altezze dei figli destro e sinistro differiscono al
massimo di 1.
-}

almostBalanced :: Ord a => BST a -> Bool
almostBalanced Void = True
almostBalanced (Node val l r) | (abs ((treeheight l) - (treeheight l)) ) > 1 = False
                              | True = ((almostBalanced l) && (almostBalanced r))

-- Esercizio 13 
-- Si scriva una funzione che dato un BST ne restituisce la lista degli elementi ottenuti visitando l’albero a livelli.
visitaPerLivelli :: Ord a => BST a -> [[a]]
visitaPerLivelli Void         = []
visitaPerLivelli (Node val l r) = (val:[]) : ( merge visit_l visit_r )
                                    where
                                        visit_l = visitaPerLivelli l
                                        visit_r = visitaPerLivelli r
merge :: [[a]] -> [[a]] -> [[a]]
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys) = (x ++ y) : merge xs ys

{- foldr liste
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
-} 

fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void = z
fold f z ( Node val l r) = f val ( fold f z l) ( fold f z r)

-- Esercizio 14 calcolare l'altezza di un albero con fold
treeheight :: Ord a => BST a -> Int
treeheight = fold (\ _ h_l h_r -> 1 + (max h_l h_r) ) (-1)

tree = Node {val=8, l=Node {val=2, l=Void, r=Node {val=5, l=Void, r=Void}}, r=Node {val=10, l=Void, r=Void}}

tree2 = Node {val=8, l=Node {val=2, l=Void, r=Node {val=5, l=Void, r=Void}}, r=Node {val=50, l=Void, r=Void}}





-------------------------------------------------------
-----------------  Esercizi Tree ----------------------
-------------------------------------------------------

data GenericTree a = VoidT | NodeT a [ GenericTree a] deriving (Eq , Show )

-- Esercizio 1. Si scriva una generalizzazione della funzione foldr delle liste per Alberi Generici
treefold :: (a->[b]->b) -> b -> GenericTree a -> b
treefold _ z VoidT = z
treefold f z (NodeT x branches) = f x (map (treefold f z) branches)

-- Esercizio 2. Si scriva una funzione height per calcolare l’altezza di un albero usando opportunamente la treefold 
treeHeight :: GenericTree a -> Int
treeHeight = treefold (\_ heights -> 1 + foldr max (-1) heights ) (-1)

-- Esercizio 7 Si scriva una funzione degree che restituisce il grado di un albero (il massimo del numero di ﬁgli per ogni nodo)

countSons :: GenericTree a -> Int
countSons VoidT = 0
countSons (NodeT a []) = 0
countSons (NodeT a (x:xs)) = 1 + countSons (NodeT a xs)

degree :: GenericTree a -> Int
degree (NodeT a xs) = degreeAux (NodeT a xs) where 
                degreeAux VoidT = 0
                degreeAux (NodeT a []) = 0 
                degreeAux (NodeT a (x:xs)) = max (max (countSons x) (degreeAux x)) (countSons (NodeT a xs))


_TREE_A :: GenericTree Int
_TREE_A = NodeT 10 [VoidT, VoidT , NodeT 5 [VoidT, VoidT] , NodeT 9 [ NodeT 23 [VoidT] ]]



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


{-
funzioneParziale f [] = []
funzioneParziale f xs = map (\x -> applicaF x f) xs

applicaF [] x = Nothing
applicaF ((v:r):xs) x | v == x = Just r
                      | True = applicaF xs x

filterMaybe [] = []
filterMaybe (x:xs) | x == Nothing = filterMaybe xs
                   | True = x : filterMaybe xs  

maxMinRow [] = []
maxMinRow xs = map (\x -> (minimum x, maximum x)) xs
-}