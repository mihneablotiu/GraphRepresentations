module AlgebraicGraph where

import qualified Data.Set as S
import Control.Arrow (ArrowChoice(right))

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node a) = S.fromList [a]
nodes (Overlay x y) = S.union (nodes x) (nodes y)
nodes (Connect x y) = S.union (nodes x) (nodes y)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node a) = S.empty
edges (Overlay x y) = S.union (edges x) (edges y)
edges (Connect x y) = S.union (S.cartesianProduct (nodes x) (nodes y)) (S.union (edges x) (edges y))

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node Empty = S.empty
outNeighbors node (Node a) = S.empty
outNeighbors node (Overlay x y) = S.union (outNeighbors node x) (outNeighbors node y)
outNeighbors node (Connect x y) = S.union
                                  (S.map snd $ S.filter (\(x, y) -> x == node) $ S.cartesianProduct (nodes x) (nodes y))
                                  $ S.union (outNeighbors node x) (outNeighbors node y)

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node Empty = S.empty
inNeighbors node (Node a) = S.empty
inNeighbors node (Overlay x y) = S.union (inNeighbors node x) (inNeighbors node y)
inNeighbors node (Connect x y) = S.union
                                 (S.map fst $ S.filter (\(x, y) -> y == node) $ S.cartesianProduct (nodes x) (nodes y))
                                 $ S.union (inNeighbors node x) (inNeighbors node y)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}

removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node Empty = Empty
removeNode node (Node a)
    | node == a = Empty
    | otherwise = Node a
removeNode node (Overlay x y) = Overlay (removeNode node x) (removeNode node y)
removeNode node (Connect x y) = Connect (removeNode node x) (removeNode node y)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news Empty = Empty
splitNode old news (Node a)
    | old == a = foldl Overlay Empty $ map (Connect Empty . Node) news
    | otherwise = Node a
splitNode old news (Overlay x y) = Overlay (splitNode old news x) (splitNode old news y)
splitNode old news (Connect x y) = Connect (splitNode old news x) (splitNode old news y)
{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node Empty = Empty
mergeNodes prop node (Node a)
    | prop a = foldl Connect Empty [Node node]
    | otherwise = Node a
mergeNodes prop node (Overlay x y) = Overlay (mergeNodes prop node x) (mergeNodes prop node y)
mergeNodes prop node (Connect x y) = Connect (mergeNodes prop node x) (mergeNodes prop node y)
