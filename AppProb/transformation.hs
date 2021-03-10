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

     Note: Parsing relies on the "Parsec" library.

     Author: Alexander Vandenbroucke (alexander.vandenbroucke@kuleuven.be)
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

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
  | IfThenElse LC LC LC
    -- ^ If-Then-Else
  deriving Show

-- | Smart constructor for true literal.
ltrue :: LC
ltrue = Lit True

-- | Smart constructor for false literal.
lfalse :: LC
lfalse = Lit False

-------------------------------------------------------------------------------
-- (Unnamed) Lambda Calculus syntax extended with booleans and boolean
-- operators.

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
  | UIfThenElse ULC ULC ULC
    -- ^ If-Then-Else
  deriving Show

type NamingContext = [(String,Int)]

-- | Turn a named ('LC') term into an equivalent nameless term ('ULC') using
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
nameless (IfThenElse tc t1 t2) ctx = UIfThenElse utc ut1 ut2 where
  utc = nameless tc ctx
  ut1 = nameless t1 ctx
  ut2 = nameless t2 ctx

-------------------------------------------------------------------------------
-- Pretty printing of named and unnamed lambda terms.

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
prettyprint (IfThenElse c x y) =
  let sc = prettyprint c
      sx = prettyprint x
      sy = prettyprint y
  in "if " ++ sc ++ " then " ++ sx ++ " else " ++ sy

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
prettyprintU (UIfThenElse c x y) =
  let sc = prettyprintU c
      sx = prettyprintU x
      sy = prettyprintU y
  in "if " ++ sc ++ " then " ++ sx ++ " else " ++ sy


-- | Serialise an unnamed lambda term to a Prolog term.
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
prologprint (UIfThenElse tc t1 t2) =
  "ite(" ++ intercalate "," [ prologprint t | t <- [tc,t1,t2] ] ++ ")"

-------------------------------------------------------------------------------
-- Parsing named lambda terms

-- | A 'Parser' for named lambda terms.
lcParser :: Parser LC
lcParser =     try appParser
           <|> try andParser
           <|> try orParser
           <|> try trueParser
           <|> try falseParser
           <|> ifthenelseParser
           <|> notParser
           <|> lamParser
           <|> varParser


lamParser, varParser, appParser, andParser, orParser, notParser, trueParser, falseParser, ifthenelseParser :: Parser LC

lamParser = Lam <$> (char '\\' *> many1 letter) <*> (char '.' *> lcParser)
varParser = Var <$> many1 letter

appParser = parens $ App <$> lcParser <*> (char ' ' *> lcParser)
andParser = parens $ And <$> lcParser <*> (string " & " *> lcParser)
orParser  = parens $ Or  <$> lcParser <*> (string " | " *> lcParser)
notParser = Not <$> (char '~' *> lcParser)
trueParser  = string "true"  *> return (Lit True)
falseParser = string "false" *> return (Lit False)

ifthenelseParser =
  IfThenElse
  <$> (string "if "    *> lcParser)
  <*> (string " then " *> lcParser)
  <*> (string " else " *> lcParser)

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
parseULC str = nameless (parseLC str) []

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
shift (UIfThenElse tc t1 t2) places cutoff = UIfThenElse tc' t1' t2' where
  tc' = shift tc places cutoff
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
subst (UIfThenElse tc t1 t2) j s =
  UIfThenElse (subst tc j s) (subst t1 j s) (subst t2 j s)
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
step (UIfThenElse tc t1 t2) = case tc of
  ULam _ -> error "step: Unexpected abstraction in if condition."
  ULit b -> if not (isValue t1) then
              UIfThenElse tc (step t1) t2
            else
              if not (isValue t2) then
                UIfThenElse tc t1 (step t2)
              else
                if b then t1 else t2
  _ -> UIfThenElse (step tc) t1 t2
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

-- | Translate a straight-line probabilistic program into a 'String'
--   representing a ProbLog program. The first argument is the pure function,
--   the second argument is a list of probabilities.
transU :: ULC -> [Prob] -> String
transU ulc probs = facts ++ program ++ query where
  n = length probs
  --
  facts :: String
  facts = concat $ zipWith fact [0..] probs
  --
  fact :: Int -> Double -> String
  fact i p = show p ++ " :: f" ++ show i ++ ".\n"
  --
  applications :: String -> String
  applications s = foldr application s [(n-1),(n-2)..0]
  --
  application :: Int -> String -> String
  application i s = "app(" ++ s ++ ",X" ++ show i ++ ")"
  --
  factswitches :: String
  factswitches = intercalate ",\n\t" [ factswitch i | i <- [0..(n-1)] ]
  --
  factswitch :: Int -> String
  factswitch i =
    let s = show i
    in "( f" ++ s ++ ", X" ++ s ++ "=t; not(f" ++ s ++ "), X" ++ s ++ "=f )"
  --
  program =
    "prog(T) :-\n"
    ++ "\tP = " ++ applications (prologprint ulc) ++ ",\n"
    ++ "\t" ++ factswitches ++ ",\n"
    ++ "\teval(P,T).\n"
  --
  query = "query(prog(T))."

-- | Translate a straigt-line probabilistic program into a 'String'
--   representing a ProbLog program that includes an evaluator. The arguments
--   are identical to 'transU'.
transUwithEval :: ULC -> [Prob] -> IO String
transUwithEval ulc probs = do
  evalpl <- readFile "eval.pl"
  return $ evalpl ++ transU ulc probs
