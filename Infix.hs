{-# OPTIONS -Wall #-}
module Infix where

import Data.Char (isDigit)
import Debug.Trace (trace)
main :: IO ()
main =  do
    expression <- getLine
    let evaluate = eval . parserInfix . lexerInfix
    print $ evaluate expression

data TokenInfix = TNum Int | TPlus | TTimes | TLeftP | TRightP
    deriving (Eq, Ord, Show)

lexerInfix :: String -> [TokenInfix]
lexerInfix "" = []
lexerInfix (' ' : cs) = lexerInfix cs
lexerInfix ('(' : cs) = TLeftP : lexerInfix cs
lexerInfix (')' : cs) = TRightP : lexerInfix cs
lexerInfix ('+' : cs) = TPlus : lexerInfix cs
lexerInfix ('*' : cs) = TTimes : lexerInfix cs
lexerInfix (c : cs)
  | isDigit c =
      let (digs, rest) = span isDigit cs
      in TNum (read (c : digs)) : lexerInfix rest
  | otherwise =
      error ("Unexpected input at " ++ show c)

data Expr = Num Int | Plus Expr Expr | Times Expr Expr | Paren Expr
  deriving (Show)


parserInfix :: [TokenInfix] -> Expr
parserInfix = parserInfix' [] []


data Symbol = SPlus | STimes | SLeftP | SRightP deriving (Eq, Ord, Show)

tokenMapping :: TokenInfix -> Symbol
tokenMapping token | token == TPlus = SPlus
                    | token == TTimes = STimes


exprMapping :: Symbol -> Expr -> Expr -> Expr
exprMapping symbol expr1 expr2 | symbol == SPlus = Plus expr1 expr2
                               | symbol == STimes = Times expr1 expr2

parserInfix' :: [Expr] -> [Symbol] -> [TokenInfix] -> Expr
parserInfix' [e] [] [] = e
parserInfix' eStack (x:xs)  [] = 
    parserInfix' (expr:restExpr) xs []
    where
        fstExpr : sndExpr : restExpr = eStack
        expr = exprMapping x fstExpr sndExpr
parserInfix' eStack sStack (token : tokens) = case token of
    TNum n -> parserInfix' (Num n : eStack) sStack tokens
    TLeftP -> parserInfix' eStack (SLeftP:sStack) tokens
    TRightP -> parserInfix' eStack' sStack' tokens'
        where
            sStack' = tail sStack
            eStack' = if head sStack == SLeftP then Paren (head eStack) : tail eStack
            else 
                let fstExpr : sndExpr : restExpr = eStack
                    symbol = head sStack
                    expr = exprMapping symbol fstExpr sndExpr
                in
                    expr:restExpr
            tokens' = if head sStack == SLeftP then tokens else token : tokens
    sym -> let 
                newSymbol = tokenMapping sym 
            in
                case sStack of
                [] -> parserInfix' eStack (newSymbol : sStack) tokens
                (symbol : _) -> case symbol of
                    SLeftP -> parserInfix' eStack (newSymbol : sStack) tokens
                    SRightP -> parserInfix' eStack (newSymbol : sStack) tokens
                    currSymbol-> if currSymbol < newSymbol
                        then
                            parserInfix' eStack (newSymbol:sStack) tokens
                        else 
                            let 
                                fstExpr : sndExpr : restExpr = trace (show eStack) eStack
                                expr = exprMapping symbol fstExpr sndExpr
                            in
                                parserInfix' (expr: restExpr) (tail sStack) (TPlus : tokens)
parserInfix' eStack sStack tokens = error (show eStack ++ show sStack ++ show tokens)

eval :: Expr -> Int
eval (Num n) = n
eval (Plus e1 e2) = eval e1 + eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Paren e) = eval e
eval _ =error "implement me"