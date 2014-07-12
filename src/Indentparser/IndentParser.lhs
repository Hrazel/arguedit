> module Main where

First, import all the needed modules.

> import Text.Parsec hiding (State)
> import Text.Parsec.Indent
> import Control.Monad.State

Next, define our new Parser type. This replaces the Identity monad
with the (State SourcePos) monad.

> type IParser a = ParsecT String () (State SourcePos) a

Now we define our new parse function. This one accepts an IParser
(which we've just defined) instead of a Parser.

> iParse :: IParser a -> SourceName -> String -> Either ParseError a
> iParse aParser source_name input =
>   runIndent source_name $ runParserT aParser () source_name input

Define our sample input string. Note: the unlines function joins
strings together with newline characters.

> input_text :: String
> input_text = unlines [
>     "listName:",
>     "  item1",
>     "  item2",
>     "  item3"
>   ]

Define main. It parses the input text and prints the parsed value. If
there was an error, it prints the error.

> main :: IO ()
> main = do
>   input <- readFile "test.txt"
>   case iParse aNamedList "indented_example" input  of
>     Left  err    -> print err
>     Right result -> putStrLn $ "I parsed: " ++ show result

Define a datatype to hold our parsed value.

> data NamedList = NamedList Name [Item]
>   deriving (Show)

Define what we mean by 'Name' and 'Item'. In this case, they are both
strings.

> type Name = String
> type Item = String

Define how we parse a NamedList. A Named list is a Name and a list of
Items contained in the NamedList data structure. Read more about the
withBlock function here:

http://hackage.haskell.org/packages/archive/indents/0.3.2/doc/html/Text-Parsec-Indent.html#v:withBlock

> aNamedList :: IParser NamedList
> aNamedList = do
>   b <- withBlock NamedList aName anItem
>   spaces
>   return b

A name is an alpha-numeric string followed by a ':' and some
whitespace.

> aName :: IParser Name
> aName = do
>   s <- many1 alphaNum
>   _ <- char ':'
>   spaces
>   return s

An item is an alpha-numeric string followed by some whitespace.

> anItem :: IParser Item
> anItem = do
>   i <- many1 alphaNum
>   spaces
>   return i

Output:
 > runhaskell -Wall indented_parsec_example.hs
 I parsed: NamedList "listName" ["item1","item2","item3"]

