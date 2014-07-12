module ArguParser (parseArgumentation,parseSpeechFile,parseDialogueFile) where
{- this is a bad copy file -}

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State
import Specification.ArguModel as ArguModel
--import System.IO.UTF8 as UTF8

{- Example file to parse
 
These: Es gibt keinen Grund Unfreunden Nutzen zu spenden.
  Definiere: Unfreund als ein nicht Freund seiender
  G: Gegenbeispiel 
    P1: Der Lehrer des Arztes meiner Kinder ist ein Unfreund.
    P2: Der Lehrer des Arztes meiner Kinder nützt mir indirekt.
    P3: Wenn ich dem Lehrer des Arztes meiner Kinder nütze, nützt das auch mir.
    Logik: P1 && P2 && P3 => Es gibt Grund: Einem Unfreund Nutzen zu spenden. 
These: Wissen ist niemals schlecht.
-}


type IParser a = ParsecT String () (State SourcePos) a
iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndent source_name $ runParserT aParser () source_name input

{- Vorlage
parseFile filename = do
  input <- readFile filename
  return $ iParse (many anArgComp) "indented_argumentation" input
-}

parseArgumentation filename = do
  input <- readFile filename
  return $ iParse (many anArgComp) "indented_argumentation" input

anArgComp :: IParser ArgComp
anArgComp = do
  b <- withBlock ArgComp aHeader anArgComp
  spaces
  return b

aLblAttr :: IParser Attribute
aLblAttr = do 
    lbs <- string "LBL"
    s <- many1 anyChar 
    return . Label s
{-aLb
aAthAttr :: IParser Attribute
aIDAttr :: IParser Attribute
aEStatAttr :: IParser Attribute
-}
anAttr :: IParser Attribute
anAttr = aLblAttr -- <|> aAthAttr  <|> aIDAttr <|> aEStatAttr )

anAttrLst :: IParser Attribute
anAttrLst = do
   commaSep = anAttr `sepBy` (symbol ",") 

aMeta :: IParser Meta
aMeta = do
   --meta <- string "Meta"
   attrs <- between (symbol "{") (symbol "}") anAttrLst
   return . Meta attrs

aHeader :: IParser Header
aHeader = (aThesis <|> aDef <|> aPremise <|> aLogic <|> aCounterExample)

aThesis :: IParser Header 
aThesis = do
  -- line
  th <- string "These"
  --headerType <- many1 alphaNum
  id <- many alphaNum
--  o_meta <- optional aMeta
  testlbl <- many aLblAttr
  _ <- char ':'
  spaces
  s <- manyTill anyChar $ char '\n'
  spaces
--  o_meta <- optional aMeta
  return . ArgHeader NoMeta $ Thesis $ Text s


aPremise :: IParser Header
aPremise = do
  th <- string "P"
  --headerType <- many1 alphaNum
  id <- many alphaNum
  _ <- char ':'
  spaces
  s <- manyTill anyChar $ char '\n'
  spaces
  return . ArgHeader NoMeta $ Premise $ Text s

 
aLogic :: IParser Header
aLogic = do
  th <- string "Logik"
  --headerType <- many1 alphaNum
  id <- many alphaNum
  _ <- char ':'
  spaces
  s <- manyTill anyChar $ char '\n'
  spaces
  return . ArgHeader NoMeta $ Logic $ Text s


{-
- G: <label text>
-   P0: <string>
-   P..: <string>
-   Pn: <string>
-   Logik: LogikExpr
-   
-}
aCounterExample :: IParser Header
aCounterExample = do
  th <- string "G"
  --headerType <- many1 alphaNum
  id <- many alphaNum
  _ <- char ':'
  spaces
  s <- manyTill anyChar $ char '\n'
  spaces
  return . ArgHeader NoMeta $ CounterExample $ Text s


{-
 -  - Definiere: <term> als <single line string>
-} 
aDefOp = ( string "=" ) <|> ( string "==" ) <|> (string "<" ) <|> ( string "/=" ) <|> (string "als") <|> (string "as")  

aDef :: IParser Header
aDef = do
  define <- (string "Definiere") <|> (string "definiere")
  id <- many alphaNum
  _ <- char ':'
  spaces
  term <- many1 alphaNum -- anyChar 
  spaces
  op <- aDefOp
  spaces
  dscr <- manyTill anyChar $ char '\n'
  spaces
  return . ArgHeader $ Definition ( Term term )  ( Descr dscr )


{-
Dialog: Platons Theaitetos
    Rede: Sokrates
        Gedenkst du nun, Theaitetos, nach diesem wiederum mit anderem schwanger zu werden, so wirst du, wenn du es wirst, dann Besseres bei    dir tragen, vermöge der gegenwärtigen Prüfung, wenn du aber leer  bleibst, denen, welche dich umgeben, weniger beschwerlich sein und sanftmütiger, [C] und mit Besonnenheit nicht glauben zu wissen, was du nicht weißt. 
        Denn nur so viel vermag diese meine Kunst, mehr aber nicht, noch verstehe ich so etwas wie die andern großen und bewunderten Männer von heute und von früher. 
        Diese geburtshelferische Kunst aber ist meiner Mutter und mir von Gott zugeteilt worden, ihr nämlich für die Frauen, und mir für begabte und ehrbare Jünglinge. 
        [D] Jetzt nun muß ich mich in der Königshalle einstellen wegen der Klage, welche Meletos gegen mich angestellt hat. 
        Morgen aber, Theaitetos, wollen wir uns wieder hier treffen
    Rede: DerSprecher
        Und so endete das Märchen
-}

parseDialogueFile filename = do
  input <- readFile filename
  return $ iParse (many parseDialogue) "indented_dialogue" input

parseSpeechFile filename = do
  input <- readFile filename
  return $ iParse (many parseSpeech) "indented_speech" input


{-
anArgComp :: IParser ArgComp
anArgComp = do
  b <- withBlock ArgComp aHeader anArgComp
  spaces
  return b
-}


parseDialogue :: IParser Dialogue
parseDialogue = do
--  dialogue <- string "Dialog"
--  headerType <- many1 alphaNum
--  id <- many alphaNum
--  _ <- char ':'
--  spaces
  b <- withBlock Dialogue (parseAttribute "Dialog") parseSpeech
--  s <- manyTill anyChar $ char '\n'
  spaces
  return b -- .HeaderDL () $ Text s


parseAttribute :: String -> IParser Attribute
parseAttribute attrname = do
   speech <- string attrname
   id <- many alphaNum
   _ <- char ':'
   spaces
   attrs <- manyTill anyChar $ char '\n'
   spaces
   return . Label $ attrs


parseSpeech :: IParser Speech
parseSpeech = do
--    speech <- string "Rede"
--    id <- many alphaNum
--    _ <- char ':'
--    theSpeaker <- manyTill anyChar $ char '\n'     
--    return Speech theSpeaker (Text "")
    b <- withBlock Speech (parseAttribute "Rede") aTextLine
    spaces
    return b


aTextLine :: IParser Text
aTextLine = do
--    zeile <- string "Zeile"
--    id <- many alphaNum
--    _ <- char ':'
--    spaces
    line <- manyTill anyChar $ char '\n'
    spaces
    return . Text $ line



