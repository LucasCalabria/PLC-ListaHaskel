{-
:cd C:\Users\Lucas\Desktop\Scripts\Haskel
:load Lista2
-}

{-
data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read)

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0
alturaArvore (Node _ n1 n2) = 1 + max (alturaArvore n1) (alturaArvore n2)
-}
------------------------------------------------------------
{-
data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)

evalTree :: IntTree -> Int
evalTree (Nilt x) = x 
evalTree (Node SUM n1 n2) = (evalTree n1) + (evalTree n2)
evalTree (Node SUB n1 n2) = (evalTree n1) - (evalTree n2)
evalTree (Node MUL n1 n2) = (evalTree n1) * (evalTree n2)
-}
--------------------------------------------------------------
{-
data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

faces :: Direction -> [Command] -> Direction
faces dir [] = dir
faces dir ((Forward  _):as) = faces dir as
faces dir ((Backward _):as) = faces dir as
faces dir ((TurnLeft  ):as) = faces (moveLeft  dir) as
faces dir ((TurnRight ):as) = faces (moveRight dir) as

moveRight :: Direction -> Direction
moveRight North = East
moveRight East  = South
moveRight South = West
moveRight West  = North

moveLeft :: Direction -> Direction
moveLeft North = West
moveLeft West  = South
moveLeft South = East
moveLeft East  = North
-}
-------------------------------------------------------------------
{-
data Tree t = Nilt |
               Node t (Tree t) (Tree t)
               deriving (Read, Show)

insertList :: Ord t => Tree t -> [t] -> Tree t
insertList tree []     = tree
insertList tree (a:as) = insertList (insertNode tree a) as

insertNode :: Ord t => Tree t -> t -> Tree t
insertNode Nilt x = Node x Nilt Nilt
insertNode (Node num n1 n2) x
    | x <= num  = (Node num (insertNode n1 x) n2)
    | otherwise = (Node num n1 (insertNode n2 x))
-}
----------------------------------------------------------------------

data Direction = North | South | West | East
                 deriving (Read, Show)

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination cord cmd = faces North cord cmd

faces :: Direction -> (Int,Int) -> [Command] -> (Int,Int)
faces _ cord [] = cord
faces dir cord ((TurnLeft ):as) = faces (moveLeft  dir) cord as
faces dir cord ((TurnRight):as) = faces (moveRight dir) cord as

faces North (x, y) ((Forward  num):as) = faces North (x, (y+num)) as
faces South (x, y) ((Forward  num):as) = faces South (x, (y-num)) as
faces West  (x, y) ((Forward  num):as) = faces West  ((x-num), y) as
faces East  (x, y) ((Forward  num):as) = faces East  ((x+num), y) as

faces North (x, y) ((Backward num):as) = faces North (x, (y-num)) as
faces South (x, y) ((Backward num):as) = faces South (x, (y+num)) as
faces West  (x, y) ((Backward num):as) = faces West  ((x+num), y) as
faces East  (x, y) ((Backward num):as) = faces East  ((x-num), y) as

moveRight :: Direction -> Direction
moveRight North = East
moveRight East  = South
moveRight South = West
moveRight West  = North

moveLeft :: Direction -> Direction
moveLeft North = West
moveLeft West  = South
moveLeft South = East
moveLeft East  = North

--------------------------------------------------------------------------------------

data Tree t = Nilt |
               Node t (Tree t) (Tree t)
               deriving (Read)

isBST :: Ord t => Tree t -> Bool
isBST Nilt               = True
isBST (Node _ Nilt Nilt) = True

isBST (Node x Nilt r)
    | x < (minOrd  r)  = isBST r
    | otherwise        = False

isBST (Node x l Nilt)
    | x >= (maxOrd l) = isBST l
    | otherwise       = False

isBST (Node x l r)
    | x < (minOrd r) && x >= (maxOrd l) = (isBST l) && (isBST r)
    | otherwise                         = False

maxOrd :: Ord t => Tree t -> t
maxOrd (Node x Nilt Nilt) = x
maxOrd (Node x Nilt r)    = max x (maxOrd r)
maxOrd (Node x l Nilt)    = max x (maxOrd l)
maxOrd (Node x l r)       = max x (max (maxOrd l) (maxOrd r))

minOrd :: Ord t => Tree t -> t
minOrd (Node x Nilt Nilt) = x
minOrd (Node x Nilt r)    = min x (minOrd r)
minOrd (Node x l Nilt)    = min x (minOrd l)
minOrd (Node x l r)       = min x (min (minOrd l) (minOrd r))

--------------------------------------------------------------------------------------

data Cmd = Cursor Int
           | Backspace Int
           | Delete Int
           | Insert String
           deriving (Read)

editText :: String -> [Cmd] -> String
editText str cmd = editTextAux str cmd 0

editTextAux :: String -> [Cmd] -> Int -> String
editTextAux str [] _ = str
editTextAux str ((Cursor n)    : ns) c = editTextAux str ns (c + n)
editTextAux str ((Insert a)    : ns) c = editTextAux (insert str a c) ns c
editTextAux str ((Delete n)    : ns) c = editTextAux (delete str c n) ns c
editTextAux str ((Backspace n) : ns) c = editTextAux (backspace str c n) ns (c-n)

backspace :: String -> Int -> Int -> String  
backspace "" _  _ = ""
backspace str _ 0 = str
backspace (a:as) c n
    | (c-n) > 0 = a : backspace as (c-1) n
    | otherwise =     backspace as (c-1) (n-1)

delete :: String -> Int -> Int -> String           --Int1 = cursor / Int2 = quant
delete ""  _ _    = ""
delete str _ 0    = str
delete (a:as) 0 n = delete as 0 (n-1)
delete (a:as) c n = a : delete as (c-1) n

insert :: String -> String -> Int -> String
insert "" str _ = str
insert str "" _ = str
insert (a:as) (b:bs) 0 = b : insert (a:as) bs 0
insert (a:as) str n    = a : insert as str (n-1)

a = "ABCDEFGH"
b = [Cursor 6, Backspace 4, Insert "9"]