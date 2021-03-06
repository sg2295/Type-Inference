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
occurs :: Atom -> Type -> Bool -- Check if the given atom occurs in the type
occurs y (x :-> xs)
  | occurs y x = True -- Check the left part of the Type
  | otherwise = occurs y xs -- Check the right part of the Type
occurs y (At x) = y == x

findAtoms :: Type -> [Atom] -- Find the atoms occuring in the given type in an alphabetically ordered list
findAtoms xs = sortList (listAtoms xs [])
  where
    listAtoms :: Type -> [Atom] -> [Atom] -- List all of the atoms in the type
    listAtoms (x :-> xs) y = listAtoms x y ++ listAtoms xs y
    listAtoms (At x) _ = [x]
    sortList :: [Atom] -> [Atom] -- Sort the list of atoms
    sortList [] = []
    sortList (x : xs) = insertToList x (sortList xs) -- Recursively sort the tail of the list
    insertToList :: Atom -> [Atom] -> [Atom]
    insertToList x [] = [x]
    insertToList x (y : ys)
      | x > y = y : insertToList x ys -- Move further down the list
      | x == y = y : ys -- Element already exists, discard duplicate
      | otherwise = x : y : ys -- Found the correct position for x

------------------------- Type substitution

type Sub = (Atom, Type)

s1 :: Sub
s1 = ("a", At "e")

s2 :: Sub
s2 = ("e", At "b" :-> At "c")

s3 :: Sub
s3 = ("c", At "a" :-> At "a")

------------------------- Assignment 2

sub :: Sub -> Type -> Type -- Apply the substitution to the given type
sub s (x :-> xs) = sub s x :-> sub s xs -- Apply the substitution to both parts of the Type
sub (a, t) (At x)
  | a == x = t
  | otherwise = At x

subs :: [Sub] -> Type -> Type -- Apply a list of subtitutions to a type
subs [] t = t
subs (x : xs) t = sub x (subs xs t) -- Apply the substitutions (head <- tail)

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

sub_u :: Sub -> [Upair] -> [Upair] -- Applies a substitution to a list of upairs
sub_u _ [] = []
sub_u s ((t1, t2) : ts) = (sub s t1, sub s t2) : sub_u s ts -- Apply the substitution to each type

step :: State -> State -- Completes a single transition of the unification algorithm
step (s, []) = (s, [])
step (s, (At t1, At t2) : ts)
  | t1 == t2 = (s, ts) -- Case a
  | otherwise = ((t1, At t2) : s, sub_u (t1, At t2) ts)
step (s, (At t1, t2) : ts) -- Case b
  | occurs t1 t2 = error ("Step: atom " ++ t1 ++ " occurs in " ++ show t2) -- Fail case
  | otherwise = ((t1, t2) : s, sub_u (t1, t2) ts)
step (s, (t1, At t2) : ts) -- Case b
  | occurs t2 t1 = error ("Step: atom " ++ t2 ++ " occurs in " ++ show t1) -- Fail case
  | otherwise = ((t2, t1) : s, sub_u (t2, t1) ts)
step (s, (s1 :-> s2, t1 :-> t2) : ts) = (s, (s1, t1) : (s2, t2) : ts) -- Case c

unify :: [Upair] -> [Sub]
unify u = aux ([], u) -- Start the algorithm with ([], U)
  where
    aux :: State -> [Sub]
    aux (s, []) = s -- If U is empty return S
    aux state = aux (step state) -- Apply transition on the state

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

conclusion :: Derivation -> Judgement -- Get the judgement of the given derivation
conclusion (Axiom j) = j
conclusion (Abstraction j _) = j
conclusion (Application j _ _) = j

subs_ctx :: [Sub] -> Context -> Context -- Applies a list of substitutions to a context
subs_ctx _ [] = []
subs_ctx s ((v, t) : ts) = (v, subs s t) : subs_ctx s ts

subs_jdg :: [Sub] -> Judgement -> Judgement -- Applies a list of substitutions to a judgement
subs_jdg s (c, te, t) = (subs_ctx s c, te, subs s t)

subs_der :: [Sub] -> Derivation -> Derivation -- Applies a list of substitutions to a derivation
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

update :: (Var, Type) -> Context -> Context -- Update the context. (Add or update the occurence of (Var, Type) in the context)
update (v, t) [] = [(v, t)] -- If we reached the end of the context list (not in the context) add it
update (v1, t1) ((v2, t2) : cs)
  | v1 == v2 = (v1, t1) : cs -- If the pair (Var, Type) is in the context then update it
  | otherwise = (v2, t2) : update (v1, t1) cs

derive0 :: Term -> Derivation
derive0 t = aux (getCon (free t), t, At "")
  where
    getCon :: [Var] -> Context -- Create a context using the list of Vars. (Used once when aux is first called)
    getCon [] = []
    getCon (v : vs) = (v, At "") : getCon vs
    aux :: Judgement -> Derivation -- Generates an incomplete derivation from a judgement, where each type is empty (At "")
    aux (c, Variable v, _) = Axiom (c, Variable v, At "")
    aux (c, Lambda v t, _) = Abstraction (c, Lambda v t, At "") (aux (update (v, At "") c, t, At ""))
    aux (c, Apply t1 t2, _) = Application (c, Apply t1 t2, At "") (aux (c, t1, At "")) (aux (c, t2, At ""))

derive1 :: Term -> Derivation
derive1 t = aux (tail atoms) (getCon (free t) atoms, t, At (head atoms))
  where
    getCon :: [Var] -> [Atom] -> Context -- Get the context of the term (Used once when aux is first called)
    getCon [] _ = []
    getCon _ [] = error "Empty Atom list in getCon of derive1"
    getCon (v : vs) (a : as) = (v, At a) : getCon vs as
    aux :: [Atom] -> Judgement -> Derivation -- Generates an incomplete derivation from a list of atoms and a judgement
    aux (a : _) (c, Variable v, _) = Axiom (c, Variable v, At a)
    aux (a1 : a2 : as) (c, Lambda v t, _) = Abstraction (c, Lambda v t, At a1) (aux as (update (v, At a2) c, t, At a2))
    aux (a : as) (c, Apply t1 t2, _) = Application (c, Apply t1 t2, At a) (aux (odds as) (c, t1, At a)) (aux (evens as) (c, t2, At a))
    odds :: [Atom] -> [Atom]
    odds (x : _ : xs) = x : odds xs -- Get the odd elements of the list
    evens :: [Atom] -> [Atom]
    evens (_ : x : xs) = x : evens xs -- Get the even elements of the list

getContext :: Judgement -> Context -- Gets the context of a judgement
getContext (c, _, _) = c

getType :: Judgement -> Type -- Gets the type of a judgement
getType (_, _, t) = t

upairs :: Derivation -> [Upair] -- Gets the type unification pairs from an incomplete derivation
upairs (Axiom (c, Variable v, t)) = [(t, find v c)]
upairs (Abstraction (c, Lambda v _, t) d) = (t, find v (getContext (conclusion d)) :-> getType (conclusion d)) : upairs d
upairs (Application (c, _, t) d1 d2) = (getType (conclusion d1), getType (conclusion d2) :-> t) : upairs d1 ++ upairs d2

derive :: Term -> Derivation -- Gets a type derivation for a term, if one exists
derive t = subs_der (unify (upairs (derive1 t))) (derive1 t)
