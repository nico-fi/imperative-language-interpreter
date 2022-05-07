{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Control.Applicative
import Data.Char
import System.Directory (doesFileExist)



-- **************************** ENVIRONMENT ****************************


type Variable = (String, [Int])

type Env = [Variable]

readVariable :: String -> Parser [Int]
readVariable name = P (\env inp -> case lookup name env of
        Nothing  -> []
        Just ns -> [(env, ns, inp)])

updateEnv :: Variable -> Parser String
updateEnv var = P (\env inp -> [(modify env, "", inp)])
    where
        modify [] = [var]
        modify (x:xs) = if fst x == fst var then var:xs else x:modify xs

updateArray :: String -> Int -> Int -> Parser String
updateArray name ind val = P (\env inp -> case lookup name env of
        Nothing  -> []
        Just ns -> [(modify env, "", inp) | ind < length ns])
    where
        modify (x:xs) = if fst x == name then
            (name, take ind (snd x) ++ val:drop (ind + 1) (snd x)):xs
          else x:modify xs



-- ****************************** PARSER *******************************


newtype Parser a = P (Env -> String -> [(Env, a, String)])

parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P p) = p

instance Functor Parser where
        -- fmap :: (a -> b) -> Parser a -> Parser b
        fmap g p = P (\env inp -> case parse p env inp of
                        [] -> []
                        [(env, v, out)] -> [(env, g v, out)])

instance Applicative Parser where
        -- pure :: a -> Parser a
        pure v = P (\env inp -> [(env, v, inp)])
        -- <*> :: Parser (a -> b) -> Parser a -> Parser b
        pg <*> px = P (\env inp -> case parse pg env inp of
                        [] -> []
                        [(env, g, out)] -> parse (fmap g px) env out)

instance Monad Parser where
        -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        p >>= f = P (\env inp -> case parse p env inp of
                        [] -> []
                        [(env, v, out)] -> parse (f v) env out)

instance Alternative Parser where
        -- empty :: Parser a
        empty = P (\env inp -> [])
        -- (<|>) :: Parser a -> Parser a -> Parser a
        p <|> q = P (\env inp -> case parse p env inp of
                                [] -> parse q env inp
                                [(env, v, out)] -> [(env, v, out)])



-- ****************************** UTILITY ******************************


item :: Parser Char
item = P (\env inp -> case inp of
                        [] -> []
                        (x:xs) -> [(env, x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
        <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

index :: Parser Int
index = do symbol "["
           n <- aexp
           symbol "]"
           return n

arraycontent :: Parser [Int]
arraycontent = do n <- aexp
                  do symbol ","
                     ns <- arraycontent
                     return (n:ns)
                   <|> return [n]



-- *********************** ARITHMETIC EXPRESSIONS **********************


aexp :: Parser Int
aexp = do t <- aterm
          do symbol "+"
             e <- aexp
             return (t + e)
           <|> do
             symbol "-"
             e <- aexp
             return (t - e)
           <|> return t

aterm :: Parser Int
aterm = do f <- afactor
           do symbol "*"
              t <- aterm
              return (f * t)
            <|> do
              symbol "/"
              t <- aterm
              return (f `div` t)
            <|> do
              symbol "%"
              t <- aterm
              return (f `mod` t)
            <|> return f

afactor :: Parser Int
afactor = do symbol "("
             e <- aexp
             symbol ")"
             return e
           <|> do
             i <- identifier
             ns <- readVariable i
             do k <- index
                if k >= 0 && k < length ns then return (ns !! k)
                else empty
              <|> do
                symbol "."
                symbol "len"
                return (length ns)
              <|> return (head ns)
           <|> integer



-- ************************ BOOLEAN EXPRESSIONS ************************


bexp :: Parser Bool
bexp = do t <- bterm
          do symbol "||"
             e <- bexp
             return (t || e)
           <|> return t

bterm :: Parser Bool
bterm = do f <- bfactor
           do symbol "&&"
              t <- bterm
              return (f && t)
            <|> return f

bfactor :: Parser Bool
bfactor = do symbol "("
             e <- bexp
             symbol ")"
             return e
           <|> do
             symbol "!"
             f <- bfactor
             return (not f)
           <|> do
             symbol "true"
             return True
           <|> do
             symbol "false"
             return False
           <|> do
             a <- aexp
             do symbol "<="
                b <- aexp
                return (a <= b)
              <|> do
                symbol "<"
                b <- aexp
                return (a < b)
              <|> do
                symbol ">="
                b <- aexp
                return (a >= b)
              <|> do
                symbol ">"
                b <- aexp
                return (a > b)
              <|> do
                symbol "=="
                b <- aexp
                return (a == b)
              <|> do
                symbol "!="
                b <- aexp
                return (a /= b)



-- ***************************** COMMANDS ******************************


program :: Parser String
program = do command
             program
           <|> command

command :: Parser String
command = assignment
      <|> ifThenElse
      <|> while
      <|> for
      <|> do symbol "skip"
             symbol ";"
             return ""

assignment :: Parser String
assignment = do i <- identifier
                do symbol "="
                   n <- aexp
                   symbol ";"
                   updateEnv (i, [n])
                 <|> do
                   symbol "="
                   symbol "Array"
                   k <- index
                   symbol ";"
                   updateEnv (i, replicate k 0)
                 <|> do
                   symbol "="
                   symbol "["
                   ns <- arraycontent
                   symbol "]"
                   symbol ";"
                   updateEnv (i, ns)
                 <|> do
                   k <- index
                   symbol "="
                   n <- aexp
                   symbol ";"
                   updateArray i k n

ifThenElse :: Parser String
ifThenElse = do symbol "if"
                b <- bexp
                symbol "{"
                if b then do
                    program
                    symbol "}"
                    do symbol "else"
                       symbol "{"
                       consumeProgram
                       symbol "}"
                       return ""
                     <|> return ""
                else do
                    consumeProgram
                    symbol "}"
                    do symbol "else"
                       symbol "{"
                       program
                       symbol "}"
                       return ""
                     <|> return ""

while :: Parser String
while = do w <- consumeWhile
           P (\env inp -> [(env, "", w ++ inp)])
           symbol "while"
           b <- bexp
           symbol "{"
           if b then do
               program
               symbol "}"
               P (\env inp -> [(env, "", w ++ inp)])
               while
            else do
               consumeProgram
               symbol "}"
               return ""

for :: Parser String
for = do symbol "for"
         symbol "("
         assignment
         c <- consumeBexp
         symbol ";"
         i <- consumeAssignment
         symbol ")"
         a <- symbol "{"
         p <- consumeProgram
         b <- symbol "}"
         P (\env inp -> [(env, "", "while" ++ c ++ a ++ p ++ i ++ b ++ inp)])
         while



-- ************************ CONSUMING UTILITY **************************


consumeIndex :: Parser String
consumeIndex = do a <- symbol "["
                  e <- consumeAexp
                  b <- symbol "]"
                  return (a ++ e ++ b)

consumeArraycontent :: Parser String
consumeArraycontent = do e <- consumeAexp
                         do c <- symbol ","
                            s <- consumeArraycontent
                            return (e ++ c ++ s)
                          <|> return e



-- ***************** CONSUMING ARITHMETIC EXPRESSIONS ******************


consumeAexp :: Parser String
consumeAexp = do t <- consumeAterm
                 do o <- symbol "+"
                    e <- consumeAexp
                    return (t ++ o ++ e)
                  <|> do
                    o <- symbol "-"
                    e <- consumeAexp
                    return (t ++ o ++ e)
                  <|> return t

consumeAterm :: Parser String
consumeAterm = do f <- consumeAfactor
                  do o <- symbol "*"
                     t <- consumeAterm
                     return (f ++ o ++ t)
                   <|> do
                     o <- symbol "/"
                     t <- consumeAterm
                     return (f ++ o ++ t)
                   <|> do
                     o <- symbol "%"
                     t <- consumeAterm
                     return (f ++ o ++ t)
                   <|> return f

consumeAfactor :: Parser String
consumeAfactor = do a <- symbol "("
                    e <- consumeAexp
                    b <- symbol ")"
                    return (a ++ e ++ b)
                  <|> do
                    i <- identifier
                    do k <- consumeIndex
                       return (i ++ k)
                     <|> do
                       d <- symbol "."
                       l <- symbol "len"
                       return (i ++ d ++ l)
                     <|> return i
                  <|> do
                    n <- integer
                    return (show n)



-- ******************* CONSUMING BOOLEAN EXPRESSIONS *******************


consumeBexp :: Parser String
consumeBexp = do t <- consumeBterm
                 do o <- symbol "||"
                    e <- consumeBexp
                    return (t ++ o ++ e)
                  <|> return t

consumeBterm :: Parser String
consumeBterm = do f <- consumeBfactor
                  do o <- symbol "&&"
                     t <- consumeBterm
                     return (f ++ o ++ t)
                   <|> return f

consumeBfactor :: Parser String
consumeBfactor = do a <- symbol "("
                    e <- consumeBexp
                    b <- symbol ")"
                    return (a ++ e ++ b)
                  <|> do
                    o <- symbol "!"
                    f <- consumeBfactor
                    return (o ++ f)
                  <|> do
                    symbol "true"
                  <|> do
                    symbol "false"
                  <|> do
                    a <- consumeAexp
                    do o <- symbol "<="
                       b <- consumeAexp
                       return (a ++ o ++ b)
                     <|> do
                       o <- symbol "<"
                       b <- consumeAexp
                       return (a ++ o ++ b)
                     <|> do
                       o <- symbol ">="
                       b <- consumeAexp
                       return (a ++ o ++ b)
                     <|> do
                       o <- symbol ">"
                       b <- consumeAexp
                       return (a ++ o ++ b)
                     <|> do
                       o <- symbol "=="
                       b <- consumeAexp
                       return (a ++ o ++ b)
                     <|> do
                       o <- symbol "!="
                       b <- consumeAexp
                       return (a ++ o ++ b)



-- ************************ CONSUMING COMMANDS *************************


consumeProgram :: Parser String
consumeProgram = do c <- consumeCommand
                    p <- consumeProgram
                    return (c ++ p)
                  <|> consumeCommand

consumeCommand :: Parser String
consumeCommand = consumeAssignment
             <|> consumeIfThenElse
             <|> consumeWhile
             <|> consumeFor
             <|> do a <- symbol "skip"
                    b <- symbol ";"
                    return (a ++ b)

consumeAssignment :: Parser String
consumeAssignment = do i <- identifier
                       do o <- symbol "="
                          e <- consumeAexp
                          s <- symbol ";"
                          return (i ++ o ++ e ++ s)
                        <|> do
                          o <- symbol "="
                          a <- symbol "Array"
                          k <- consumeIndex
                          s <- symbol ";"
                          return (i ++ o ++ a ++ k ++ s)
                        <|> do
                          o <- symbol "="
                          a <- symbol "["
                          c <- consumeArraycontent
                          b <- symbol "]"
                          s <- symbol ";"
                          return (i ++ o ++ a ++ c ++ b ++ s)
                        <|> do
                          k <- consumeIndex
                          o <- symbol "="
                          e <- consumeAexp
                          s <- symbol ";"
                          return (i ++ k ++ o ++ e ++ s)

consumeIfThenElse :: Parser String
consumeIfThenElse = do i <- symbol "if"
                       x <- consumeBexp
                       a <- symbol "{"
                       p <- consumeProgram
                       b <- symbol "}"
                       do e <- symbol "else"
                          c <- symbol "{"
                          q <- consumeProgram
                          d <- symbol "}"
                          return (i ++ x ++ a ++ p ++ b ++ e ++ c ++ q ++ d)
                        <|> return (i ++ x ++ a ++ p ++ b)

consumeWhile :: Parser String
consumeWhile = do w <- symbol "while"
                  e <- consumeBexp
                  a <- symbol "{"
                  p <- consumeProgram
                  b <- symbol "}"
                  return (w ++ e ++ a ++ p ++ b)

consumeFor :: Parser String
consumeFor = do f <- symbol "for"
                a <- symbol "("
                i <- consumeAssignment
                e <- consumeBexp
                t <- symbol ";"
                s <- consumeAssignment
                b <- symbol ")"
                c <- symbol "{"
                p <- consumeProgram
                d <- symbol "}"
                return (f ++ a ++ i ++ e ++ t ++ s ++ b ++ c ++ p ++ d)



-- **************************** EXECUTION ******************************


main :: IO ()
main = do putStrLn "  ╭━━┳━╮╭━┳━━━╮"
          putStrLn "  ╰┫┣┫┃╰╯┃┃╭━╮┃  Haskell Interpreter for Imperative Language"
          putStrLn "  ╱┃┃┃╭╮╭╮┃╰━╯┃  [] Type some code to evaluate"
          putStrLn "  ╱┃┃┃┃┃┃┃┃╭━━╯  [] Type 'load pathToFile' to evaluate a file"
          putStrLn "  ╭┫┣┫┃┃┃┃┃┃     [] Type 'exit' to quit"
          putStrLn "  ╰━━┻╯╰╯╰┻╯\n"
          start

start :: IO ()
start = do putStr "IMP> "
           input <- getLine
           case head (words input) of
             "exit" -> return ()
             "load" -> do exists <- doesFileExist (last (words input))
                          if exists then do
                            file <- readFile (last (words input))
                            putStrLn (eval file)
                          else putStrLn "\nFile not found\n"
                          start
             _ -> do putStrLn (eval input)
                     start

eval :: String -> String
eval p = case parse program [] p of
           [(env, _, [])] ->
                    "\nParser output:\n" ++
                    (\[(_, s, _)] -> s) (parse consumeProgram [] p) ++
                    "\n\nMemory:\n" ++
                    concatMap (\(a, b) -> a ++ " = " ++ show b ++ "\n") env
           [(_, _, out)] -> "\nUnused input:\n" ++ out ++ "\n"
           [] -> "\nInvalid input\n"
