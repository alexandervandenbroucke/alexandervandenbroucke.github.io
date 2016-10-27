{- | From straight-line probabilistic applicative programs to ProbLog.

     A straight-line applicative program is a program of the following shape:

     > pure f <$> choose p1 True False <*> ... <*> choose p2 True False

     where @f@ is a pure function. We'll represent such programs by a
     lambda-term for @f@ and a list of probabilities @[p1,...,pn]@.

     The majority of this file concerns itself with implementing the machinery
     of the (named and unnamed) lambda-calculus and requisite parsing
     and pretty printing facilities. The relevant functions are 'parseLC' and
     'parseULC'.

     For example:

     > parseULC "\\x.(x x)"

     parses the lambda-term \x.(x x) and turns it into an unnamed (De Bruijn)
     representation.

     The function 'transU' turns this lambda-term and a list of probabilities
     into an equivalent ProbLog program. The function 'transUwithEval'
     additionally adds an evaluator for lambda-terms to evaluate the program
     (see the companion file eval.pl).

     For example (with -XOverloadedStrings):

     > transUwithEval (parseULC "\\y.\\x.(x & y)") [0.5,0.6] >>= putStrLn

     prints out a program that can be pasted directly into the ProbLog web-
     interface (https://dtai.cs.kuleuven.be/problog/editor.html).

     Author: Alexander Vandenbroucke (alexander.vandenbroucke@kuleuven.be)
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad (replicateM)
import Control.Monad.State (State, get, put, evalState)
import Text.Parsec.ByteString (Parser)
import Text.Parsec ((<|>), try, char, many1, letter, string, between, parse)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.List (intercalate)

-------------------------------------------------------------------------------
-- (Named) Lambda Calculus syntax extended with booleans and boolean operators

data LC
  = Lam String LC
    -- ^ Lambda abstraction
  | App LC LC
    -- ^ Application
  | Var String
    -- ^ Named variable
  | Lit Bool
    -- ^ Boolean literal
  | And LC LC
    -- ^ Conjunction
  | Or LC LC
    -- ^ Disjunction
  | Not LC
    -- ^ Negation
  deriving Show

-- | Smart constructor for true literal.
ltrue :: LC
ltrue = Lit True

-- | Smart constructor for false literal.
lfalse :: LC
lfalse = Lit False

-------------------------------------------------------------------------------
-- (Unnamed) Lambda Calculus syntax extended with booleans and boolean operators

data ULC
  = ULam ULC
    -- ^ Unnamed lambda abstraction.
  | UApp ULC ULC
    -- ^ Unnamed application
  | UVar Int
    -- ^ Unnamed variable
  | ULit Bool
    -- ^ Boolean literal
  | UAnd ULC ULC
    -- ^ Conjunction
  | UOr ULC ULC
    -- ^ Disjunction
  | UNot ULC
    -- ^ Negation
  deriving Show

type NamingContext = [(String,Int)]

-- | Turn a named (|LC|) term into an equivalent nameless term (|ULC|) using
--   de Bruijn indexing.
nameless :: LC -> NamingContext -> ULC
nameless (Lam name t) ctx = ULam (nameless t ctx') where
  ctx' = (name,0):map (second (+1)) ctx
nameless (Var v) ctx = case lookup v ctx of
  Nothing -> error $ "Unknown free variable: " ++ show v
  Just i  -> UVar i
nameless (Lit b)     _   = ULit b
nameless (App t1 t2) ctx = UApp (nameless t1 ctx) (nameless t2 ctx)
nameless (And t1 t2) ctx = UAnd (nameless t1 ctx) (nameless t2 ctx)
nameless (Or t1 t2)  ctx = UOr (nameless t1 ctx) (nameless t2 ctx)
nameless (Not t)     ctx = UNot (nameless t ctx)

-------------------------------------------------------------------------------
-- Pretty printing named and unnamed lambda terms.

-- | Pretty print a named lambda term.
prettyprint :: LC -> String
prettyprint (Lam x t) =
  let s = prettyprint t
  in "\\" ++ x ++ "." ++ s
prettyprint (App x y) =
  let sx = prettyprint x
      sy = prettyprint y
  in "(" ++ sx ++ " " ++ sy ++ ")"
prettyprint (Var x) = x
prettyprint (Lit True)  = "true"
prettyprint (Lit False) = "false"
prettyprint (And x y) =
  let sx = prettyprint x
      sy = prettyprint y
  in "(" ++ sx ++ " & " ++ sy ++ ")"
prettyprint (Or x y) =
  let sx = prettyprint x
      sy = prettyprint y
  in "(" ++ sx ++ " | " ++ sy ++ ")"
prettyprint (Not x) =
  let sx = prettyprint x
  in "~(" ++ sx ++ ")"

-- | Pretty print an unnamed lambda terms.
prettyprintU :: ULC -> String
prettyprintU (ULam t) =
  let s = prettyprintU t
  in "\\." ++ s
prettyprintU (UApp x y) =
  let sx = prettyprintU x
      sy = prettyprintU y
  in "(" ++ sx ++ " " ++ sy ++ ")"
prettyprintU (UVar x) = show x
prettyprintU (ULit True)  = "true"
prettyprintU (ULit False) = "false"
prettyprintU (UAnd x y) =
  let sx = prettyprintU x
      sy = prettyprintU y
  in "(" ++ sx ++ " & " ++ sy ++ ")"
prettyprintU (UOr x y) =
  let sx = prettyprintU x
      sy = prettyprintU y
  in "(" ++ sx ++ " | " ++ sy ++ ")"
prettyprintU (UNot x) =
  let sx = prettyprintU x
  in "~(" ++ sx ++ ")"

prologprint :: ULC -> String
prologprint (ULam t) = "lam(" ++ prologprint t ++ ")"
prologprint (UVar i) = "var(" ++ show i ++ ")"
prologprint (ULit b) = if b then "t" else "f"
prologprint (UApp t1 t2) =
  "app(" ++ prologprint t1 ++ "," ++ prologprint t2 ++ ")"
prologprint (UAnd t1 t2) =
  "and(" ++ prologprint t1 ++ "," ++ prologprint t2 ++ ")"
prologprint (UOr t1 t2) =
  "or(" ++ prologprint t1 ++ "," ++ prologprint t2 ++ ")"
prologprint (UNot t) =
  "not(" ++ prologprint t ++ ")"

-------------------------------------------------------------------------------
-- Parsing named lambda terms

-- | A 'Parser' for named lambda terms.
lcParser :: Parser LC
lcParser =     try appParser
           <|> try andParser
           <|> try orParser
           <|> try trueParser
           <|> try falseParser
           <|> notParser
           <|> lamParser
           <|> varParser


lamParser, varParser, appParser, andParser, orParser, notParser, trueParser, falseParser :: Parser LC

lamParser = Lam <$> (char '\\' *> many1 letter) <*> (char '.' *> lcParser)
varParser = Var <$> many1 letter

appParser = parens $ App <$> lcParser <*> (char ' ' *> lcParser)
andParser = parens $ And <$> lcParser <*> (string " & " *> lcParser)
orParser  = parens $ Or  <$> lcParser <*> (string " | " *> lcParser)
notParser = Not <$> (char '~' *> lcParser)
trueParser  = string "true"  *> return (Lit True)
falseParser = string "false" *> return (Lit False)

-- | A 'Parser' wrapping another 'Parser' between parentheses.
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- | Parse an named lambda term.
parseLC :: ByteString -> LC
parseLC s = case parse lamParser "" s of
  Left  e -> error $ "parseLC: " ++ show e
  Right t -> t

-- | Parse a named lambda term and turn it into an unnamed lambda term.
parseULC :: ByteString -> ULC
parseULC = flip nameless [] . parseLC
-------------------------------------------------------------------------------
-- MonadFresh

-- | A monad that can always return a fresh integer.
class Monad m => MonadFresh m where
  fresh :: m Int

instance MonadFresh (State Int) where
  fresh = do
    i <- get
    put (i + 1)
    return i

-------------------------------------------------------------------------------
-- Evaluator for unnamed lambda terms

-- | Shift unnamed variables.
--   @'shift' t places cutoff@ shifts all variables in @t@ that are greater
--   than or equal to the @cutoff@ by @places@.
shift :: ULC -> Int -> Int -> ULC
shift t@(UVar i) places cutoff | i < cutoff = t
                               | otherwise  = UVar (i + places)
shift (ULam t)     places cutoff = ULam (shift t places (cutoff+1))
shift (UApp t1 t2) places cutoff = UApp t1' t2' where
  t1' = shift t1 places cutoff
  t2' = shift t2 places cutoff
shift (UAnd t1 t2) places cutoff = UAnd t1' t2' where
  t1' = shift t1 places cutoff
  t2' = shift t2 places cutoff
shift (UOr t1 t2) places cutoff = UOr t1' t2' where
  t1' = shift t1 places cutoff
  t2' = shift t2 places cutoff
shift (UNot t) places cutoff = UNot (shift t places cutoff)
shift t@(ULit _) _ _ = t


-- | Substitute a variable in an unnamed lambda term
--   @'subst' t j s@ substitutes s for all j in t.
subst :: ULC -> Int -> ULC -> ULC
subst t@(UVar i) j s | i == j    = s
                     | otherwise = t
subst (ULam t') j s = ULam (subst t' (j+1) (shift s 1 0))
subst (UApp t1 t2) j s = UApp (subst t1 j s) (subst t2 j s)
subst (UAnd t1 t2) j s = UAnd (subst t1 j s) (subst t2 j s)
subst (UOr  t1 t2) j s = UOr  (subst t1 j s) (subst t2 j s)
subst (UNot t)     j s = UNot (subst t  j s)
subst t@(ULit _)   _ _ = t

-- | Check if an unnamed lambda term is a value (an abstraction or a literal).
isValue :: ULC -> Bool
isValue (ULam _) = True
isValue (ULit _) = True
isValue _        = False

-- | Perform a small evaluation step.
--   This procedure can return an error message if the evaluation relation is
--   undefined for the input.
step :: ULC -> ULC
step (UApp t1 t2) = case t1 of 
  ULam t12 | isValue t2 -> subst t12 0 t2
           | otherwise  -> UApp t1 (step t2)
  ULit _ -> error "step: Unexepected literal in application."
  _      -> UApp (step t1) t2
step (UAnd t1 t2) = case t1 of
  ULam _  -> error "step: Unexpected abstraction in conjunction."
  ULit b1 -> case t2 of
    ULam _  -> error "step: Unexpected abstraction in conjunction."
    ULit b2 -> ULit (b1 && b2)
    _       -> UAnd t1 (step t2)
  _  -> UAnd (step t1) t2
step (UOr t1 t2) = case t1 of
  ULam _  -> error "step: Unexpected abstraction in disjunction."
  ULit b1 -> case t2 of
    ULam _  -> error "step: Unexpected abstraction in disjunction."
    ULit b2 -> ULit (b1 || b2)
    _       -> UAnd t1 (step t2)
  _  -> UAnd (step t1) t2
step (UNot t) = case t of
  ULam _ -> error "step: Unexpected abstraction in negation."
  ULit b -> ULit (not b)
  _      -> (UNot (step t))
step t = error $ "step: step is not defined for " ++ prettyprintU t

-- | Evaluate an unnamed lambda term: apply the small step semantics until
--   a value is obtained.
eval :: ULC -> ULC
eval t =
  if isValue t then
    t
  else
    eval (step t)

-------------------------------------------------------------------------------
-- Transformation/translation of straight-line probabilistic applicative
-- programs into ProbLog

type Prob = Double

-- | Translate a straigt-line probabilistic program into a 'String'
--   representing a ProbLog program. The first argument is the pure function,
--   the second argument is a list of probabilities.
transU :: ULC -> [Prob] -> String
transU ulc probs = evalState facts (0 :: Int) ++ program ++ query where
  n = length probs
  --
  facts :: State Int String
  facts = fmap concat $ mapM fact probs
  --
  fact :: Double -> State Int String
  fact p = do
    i <- fresh
    return (show p ++ " :: f" ++ show i ++ ".\n")
  --
  applications :: String -> State Int String
  applications s =
    foldr (=<<) (return s)
    $ replicate n application
  --
  application :: String -> State Int String
  application s = do
    i <- fresh
    return $ "app(" ++ s ++ ",X" ++ show i ++ ")"
  --
  factswitches :: State Int String
  factswitches =
    fmap (intercalate ",\n\t")
    $ replicateM n
    $ factswitch
  --
  factswitch :: State Int String
  factswitch = do
    i <- fmap show fresh
    return $
      "( f" ++ i ++ ", X" ++ i ++ "=t; not(f" ++ i ++ "), X" ++ i ++ "=f )"
  --
  program =
    "prog(T) :-\n"
    ++ "\tP = " ++ evalState (applications (prologprint ulc)) 0 ++ ",\n"
    ++ "\t" ++ evalState factswitches 0 ++ ",\n"
    ++ "\teval(P,T).\n"
  --
  query = "query(prog(T))."

-- | Translate a straigt-line probabilistic program into a 'String'
--   representing a ProbLog program that includes an evaluator. The arguments
--   are identical to 'transU'.
transUwithEval :: ULC -> [Prob] -> IO String
transUwithEval ulc probs = do
  evalpl <- readFile "popl-pps-abstract/eval.pl"
  return $ evalpl ++ transU ulc probs
