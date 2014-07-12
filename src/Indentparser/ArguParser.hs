{-| 
This is the Parser for the argumentations ArguEdit gets to work with.
It also specifies the fileformat of the argumentations files *.arg .

It recognizes indentation as part of the content as definitions premises and so on are contained inside the discourse on a thesis.

The file format looks like this:
<ArgComp identifier>(optional Status <status value>): <String>
    (optional indented lines as list of items)


(Note: On load the lack of an explicit Status implies "Status Initial".)

-}
module Indentparser.ArguParser (parseArgumentation,parseSpeechFile,parseDialogueFile) where
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Token
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

parseArgumentation :: FilePath -> IO (Either ParseError [ArgComp])
parseArgumentation filename = do
  input <- readFile filename
  return $ iParse (many anArgComp) "indented_argumentation" input

anArgComp :: IParser ArgComp
anArgComp = do
  b <- withBlock ArgComp aHeader anArgComp
  spaces
  return b

aHeader :: IParser Header
aHeader = (aThesis <|> aDef <|> aPremise <|> aLogic <|> aCounterExample)

{-
aLblAttr :: IParser Attribute
aLblAttr = do 
    lbs <- string "LBL"
    s <- many1 anyChar 
    return . Label s
-}

aAthAttr :: IParser Attribute
aAthAttr = do
    auth <- try (string "Author")
    spaces
    author <- many1 (letter <|> space ) 
    return (Author author)

toStatus :: String -> Attribute
toStatus id = case id of
                "Initial" -> Status Initial
                "Valid" -> Status Valid
                "Invalid" -> Status Invalid
                "Verified" -> Status Verified
                "Proposed" -> Status Proposed
                "Falsified" -> Status Falsified


aEStatAttr :: IParser Attribute
aEStatAttr = do
   status <- (try (string "Status "))
   statValStr <- (try (string "Initial") <|> try (string "Proposed") <|> try (string "Falsified") <|> try (string "Valid") <|> try (string "Invalid") <|> try (string "Verified") )
   let res = (toStatus statValStr)   
   return res --(Status Initial)

anAttr :: IParser Attribute
anAttr = (try aEStatAttr <|> try aAthAttr ) --  <|> aIDAttr <|> aLblAttr )

anAttrLst :: IParser [Attribute]
anAttrLst =  do
    res <- (anAttr `sepBy` (char ',') )
    return res

aMeta :: IParser Meta
aMeta = do
  attrs <- try (between (char '[' ) (char ']' ) anAttrLst )
  return (Meta attrs)


parseMeta :: ParsecT String () (State SourcePos) Meta
parseMeta = do
    mb_meta <- (optionMaybe aMeta)
    let meta = case mb_meta of
                Nothing -> Meta [Status Initial]
                Just m  -> m
    return meta


aThesis :: IParser Header 
aThesis = do
  -- line
  th <- try (string "THESE") <|> try (string "THESIS")
  id <- many alphaNum
  spaces
  meta <- parseMeta
  _ <- char ':'
  spaces
  s <- manyTill anyChar $ char '\n'
  spaces
  return . ArgHeader meta $ Thesis $ Text s


aPremise :: IParser Header
aPremise = do
  premiseStr <- (try (string "PREMISE") <|> try (string "PRÄMISSE"))
  id <- many alphaNum
  spaces
  meta <- parseMeta
  _ <- char ':'
  spaces
  s <- manyTill anyChar $ char '\n'
  spaces
  return . ArgHeader meta $ Premise $ Text s

 
aLogic :: IParser Header
aLogic = do
  th <- (try (string "LOGIC") <|> try (string "SCHLUSS"))
  id <- many alphaNum
  spaces
  meta <- parseMeta
  _ <- char ':'
  spaces
  s <- manyTill anyChar $ char '\n'
  spaces
  return . ArgHeader meta $ Logic $ Text s


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
  th <- (try (string "G") <|> try (string "GEGENBEISPIEL") <|> try ( string "COUNTEREXAMPLE" ))
  id <- many alphaNum
  spaces
  meta <- parseMeta
  _ <- char ':'
  spaces
  s <- manyTill anyChar $ char '\n'
  spaces
  return . ArgHeader meta $ CounterExample $ Text s


{-
 -  - DEFINE<meta>: <term> AS <single line string>
-} 
aDefOp = (try ( string "=" ) {- <|> try  (string "<" ) <|> try  ( string "/=" )-} <|> try  (string "ALS") <|> try  (string "AS")  )

aDef :: IParser Header
aDef = do
  define <- (try (string "DEFINIERE") <|> try (string "DEFINE"))
  id <- many alphaNum
  spaces
  meta <- parseMeta
  _ <- char ':'
  spaces
  term <- manyTill anyChar (try (aDefOp))
  spaces
  dscr <- manyTill anyChar $ char '\n'
  spaces
  return . ArgHeader meta $ Definition ( Term term )  ( Descr dscr )


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

parseDialogueFile :: FilePath -> IO (Either ParseError [Dialogue])
parseDialogueFile filename = do
  input <- readFile filename
  return $ iParse (many parseDialogue) "indented_dialogue" input

parseSpeechFile :: FilePath -> IO (Either ParseError [Speech]) 
parseSpeechFile filename = do
  input <- readFile filename
  return $ iParse (many parseSpeech) "indented_speech" input


parseDialogue :: IParser Dialogue
parseDialogue = do
  b <- withBlock Dialogue (parseAttribute "Dialog") parseSpeech
  spaces
  return b 


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
    b <- withBlock Speech (parseAttribute "Rede") aTextLine
    spaces
    return b


aTextLine :: IParser Text
aTextLine = do
    line <- manyTill anyChar $ char '\n'
    spaces
    return . Text $ line



