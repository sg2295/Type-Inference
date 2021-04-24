--------------------------------------------
--                                        --
-- CM20256/CM50262 Functional Programming --
--                                        --
--         Coursework 2020/2021           --
--                                        --
--------------------------------------------

------------------------- Auxiliary functions

find :: (Show a, Eq a) => a -> [(a, b)] -> b
find x [] = error ("find: " ++ show x ++ " not found")
find x ((y, z) : zs)
  | x == y = z
  | otherwise = find x zs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x == y = x : merge xs ys
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x : xs) (y : ys)
  | x < y = x : minus xs (y : ys)
  | x == y = minus xs ys
  | otherwise = minus (x : xs) ys

------------------------- Lambda-terms

type Var = String

data Term
  = Variable Var
  | Lambda Var Term
  | Apply Term Term
  deriving (Eq)

instance Show Term where
  show = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m
      f i (Apply n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x n) = free n `minus` [x]
free (Apply n m) = free n `merge` free m

------------------------- Types

infixr 5 :->

type Atom = String

data Type = At Atom | Type :-> Type
  deriving (Eq)

instance Show Type where
  show (At a) = a
  show (At a :-> s) = a ++ " -> " ++ show s
  show (t :-> s) = "(" ++ show t ++ ") -> " ++ show s

atoms :: [Atom]
atoms = map (: []) ['a' .. 'z'] ++ [a : show i | i <- [1 ..], a <- ['a' .. 'z']]

t1 :: Type
t1 = At "a" :-> At "b"

t2 :: Type
t2 = (At "c" :-> At "d") :-> At "e"

t3 :: Type
t3 = At "a" :-> At "c" :-> At "c"

------------------------- Assignment 1
occurs :: Atom -> Type -> Bool
occurs y (x :-> xs)
  | occurs y x = True
  | otherwise = occurs y xs
occurs y (At x) = y == x

findAtoms :: Type -> [Atom] -- filter function can be used instead TODO (in a single line)
findAtoms xs = sortList (listAtoms xs [])
  where
    listAtoms :: Type -> [Atom] -> [Atom]
    listAtoms (x :-> xs) y = listAtoms x y ++ listAtoms xs y
    listAtoms (At x) y = [x]
    sortList :: [Atom] -> [Atom]
    sortList [] = []
    sortList (x : xs) = insertToList x (sortList xs) -- Recursively sort the tail
    insertToList :: Atom -> [Atom] -> [Atom]
    insertToList x [] = [x]
    insertToList x (y : ys)
      | x > y = y : insertToList x ys -- Move further down the list
      | x == y = y : ys
      | otherwise = x : y : ys -- Place the value in the current index

------------------------- Type substitution

type Sub = (Atom, Type)

s1 :: Sub
s1 = ("a", At "e")

s2 :: Sub
s2 = ("e", At "b" :-> At "c")

s3 :: Sub
s3 = ("c", At "a" :-> At "a")

------------------------- Assignment 2

sub :: Sub -> Type -> Type
sub s (x :-> xs) = sub s x :-> sub s xs
sub (a, t) (At x)
  | a == x = t
  | otherwise = At x

-- subs :: [Sub] -> Type -> Type
-- subs xs = subAtoms (reverseList xs)
--   where
--     subAtoms :: [Sub] -> Type -> Type
--     subAtoms [] t = t
--     subAtoms (x : xs) t = subAtoms xs (sub x t)
--     reverseList :: [Sub] -> [Sub]
--     reverseList = rev []
--     rev :: [Sub] -> [Sub] -> [Sub]
--     rev xs [] = xs
--     rev xs (y : ys) = rev (y : xs) ys

subs :: [Sub] -> Type -> Type
subs [] t = t
subs (x : xs) t = sub x (subs xs t)

------------------------- Unification

type Upair = (Type, Type)

type State = ([Sub], [Upair])

u1 :: Upair
u1 = (t1, At "c")

u2 :: Upair
u2 = (At "a" :-> At "a", At "a" :-> At "c")

u3 :: Upair
u3 = (t1, t2)

u4 :: Upair
u4 = (t2, t3)

st1 :: State
st1 = ([], [u1, u2])

------------------------- Assignment 3

sub_u :: Sub -> [Upair] -> [Upair]
sub_u _ [] = []
sub_u s ((t1, t2) : ts) = (sub s t1, sub s t2) : sub_u s ts

step :: State -> State
step (s, []) = (s, [])
step (s, (At t1, At t2) : ts)
  | t1 == t2 = (s, ts)
  | otherwise = ((t1, At t2) : s, sub_u (t1, At t2) ts)
step (s, (At t1, t2) : ts)
  | occurs t1 t2 = error ("Step: atom " ++ t1 ++ " occurs in " ++ show t2)
  | otherwise = ((t1, t2) : s, sub_u (t1, t2) ts)
step (s, (t1, At t2) : ts)
  | occurs t2 t1 = error ("Step: atom " ++ t2 ++ " occurs in " ++ show t1)
  | otherwise = ((t2, t1) : s, sub_u (t2, t1) ts)
step (s, (s1 :-> s2, t1 :-> t2) : ts) = (s, (s1, t1) : (s2, t2) : ts)

unify :: [Upair] -> [Sub]
unify u = unifyHelper ([], u)
  where
    unifyHelper :: State -> [Sub]
    unifyHelper (s, []) = s
    unifyHelper state = unifyHelper (step state)

------------------------- Assignment 4

type Context = [(Var, Type)]

type Judgement = (Context, Term, Type)

data Derivation
  = Axiom Judgement
  | Abstraction Judgement Derivation
  | Application Judgement Derivation Derivation
  deriving (Eq)

n1 = Apply (Lambda "x" (Variable "x")) (Variable "y")

d1 =
  Application
    ([("y", At "a")], n1, At "a")
    ( Abstraction
        ([("y", At "a")], Lambda "x" (Variable "x"), At "a" :-> At "a")
        ( Axiom ([("x", At "a"), ("y", At "a")], Variable "x", At "a")
        )
    )
    ( Axiom ([("y", At "a")], Variable "y", At "a")
    )

d2 =
  Application
    ([("y", At "b")], Apply (Lambda "x" (Apply (Variable "x") (Variable "y"))) (Lambda "z" (Variable "z")), At "a")
    ( Abstraction
        ([("y", At "b")], Lambda "x" (Apply (Variable "x") (Variable "y")), At "c")
        ( Application
            ([("x", At "d"), ("y", At "b")], Apply (Variable "x") (Variable "y"), At "e")
            ( Axiom ([("x", At "d"), ("y", At "b")], Variable "x", At "f")
            )
            ( Axiom ([("x", At "d"), ("y", At "b")], Variable "y", At "g")
            )
        )
    )
    ( Abstraction
        ([("y", At "b")], Lambda "z" (Variable "z"), At "h")
        ( Axiom ([("z", At "i"), ("y", At "b")], Variable "z", At "j")
        )
    )

-- Type definitions:
-- State = ([Sub], [Upair])
-- Sub = (Atom, Type)
-- Upair = (Type, Type)
-- New:
-- type Context = [(Var, Type)]
-- type Judgement = (Context, Term, Type)
-- data Derivation
--   = Axiom Judgement
--   | Abstraction Judgement Derivation
--   | Application Judgement Derivation Derivation
conclusion :: Derivation -> Judgement
conclusion (Axiom j) = j
conclusion (Abstraction j _) = j
conclusion (Application j _ _) = j

subs_ctx :: [Sub] -> Context -> Context
subs_ctx _ [] = []
subs_ctx s ((v, t) : ts) = (v, subs s t) : subs_ctx s ts

subs_jdg :: [Sub] -> Judgement -> Judgement
subs_jdg s (c, te, t) = (subs_ctx s c, te, subs s t)

subs_der :: [Sub] -> Derivation -> Derivation
subs_der s (Axiom j) = Axiom (subs_jdg s j)
subs_der s (Abstraction j d) = Abstraction (subs_jdg s j) (subs_der s d)
subs_der s (Application j d1 d2) = Application (subs_jdg s j) (subs_der s d1) (subs_der s d2)

------------------------- Typesetting derivations

instance Show Derivation where
  show d = unlines (reverse strs)
    where
      (_, _, _, strs) = showD d

      showC :: Context -> String
      showC [] = []
      showC [(x, t)] = x ++ ": " ++ show t
      showC ((x, t) : cx) = x ++ ": " ++ show t ++ " , " ++ showC cx

      showJ :: Judgement -> String
      showJ ([], n, t) = "|- " ++ show n ++ " : " ++ show t
      showJ (cx, n, t) = showC cx ++ " |- " ++ show n ++ " : " ++ show t

      showL :: Int -> Int -> Int -> String
      showL l m r = replicate l ' ' ++ replicate m '-' ++ replicate r ' '

      showD :: Derivation -> (Int, Int, Int, [String])
      showD (Axiom j) = (0, k, 0, [s, showL 0 k 0]) where s = showJ j; k = length s
      showD (Abstraction j d) = addrule (showJ j) (showD d)
      showD (Application j d e) = addrule (showJ j) (sidebyside (showD d) (showD e))

      addrule :: String -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
      addrule x (l, m, r, xs)
        | k <= m = (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL l m r : xs)
        | k <= l + m + r = (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL ll k rr : xs)
        | otherwise = (0, k, 0, x : replicate k '-' : [replicate (- ll) ' ' ++ y ++ replicate (- rr) ' ' | y <- xs])
        where
          k = length x
          i = div (m - k) 2
          ll = l + i
          rr = r + m - k - i

      extend :: Int -> [String] -> [String]
      extend i strs = strs ++ repeat (replicate i ' ')

      sidebyside :: (Int, Int, Int, [String]) -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
      sidebyside (l1, m1, r1, d1) (l2, m2, r2, d2)
        | length d1 > length d2 = (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ "  " ++ y | (x, y) <- zip d1 (extend (l2 + m2 + r2) d2)])
        | otherwise = (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ "  " ++ y | (x, y) <- zip (extend (l1 + m1 + r1) d1) d2])

------------------------- Assignment 5

derive0 :: Term -> Derivation
derive0 = undefined
  where
    aux :: Judgement -> Derivation
    aux = undefined

derive1 :: Term -> Derivation
derive1 = undefined
  where
    aux :: [Atom] -> Judgement -> Derivation
    aux = undefined

upairs :: Derivation -> [Upair]
upairs = undefined

derive :: Term -> Derivation
derive = undefined
