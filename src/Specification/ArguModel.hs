{- |
This the core model of ArguEdit.
Basic structure of the transcription of argumentations as well as fundamental 
operations on them can be found here.


-}

module  Specification.ArguModel
    where
    import Data.Maybe
    import Data.Tree    
    import System.IO
    import Control.Monad.State
    import qualified Data.Map.Strict as Map
    import qualified Data.Bimap as Bimap
--    import Data.Unique.Id    
    
--types
    data ImgLetter = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving (Eq,Show,Enum)
    data ImgColor =  Black | Blue | DarkGreen | LightGreen |
                     Gold | Grey | Orange | Pink | Red | 
                     Violet deriving (Eq,Show,Enum,Ord)
    availableColors :: [ImgColor]
    availableColors = [Grey,Blue,LightGreen,Red,Black]

    statColorMap :: Bimap.Bimap ImgColor EStatus
    statColorMap = Bimap.fromList [(Grey,Initial),(Blue,Valid),(LightGreen,Verified),(Red,Falsified),(Black,Proposed)]

    whichColor :: Monad m => EStatus -> m ImgColor
    whichColor stat = Bimap.lookupR stat statColorMap

    whichEStatus :: Monad m => ImgColor -> m EStatus
    whichEStatus c = Bimap.lookup c statColorMap
    
    updateListAttribute a l = map (replaceAttr a) l

    replaceAttr :: Attribute -> Attribute -> Attribute
    replaceAttr (Status e) (Status _) = Status e
    replaceAttr (Label s) (Label _) = Label s
    replaceAttr (Author a) (Author _) = Author a
    replaceAttr (ID id) (ID _) = ID id
    replaceAttr _ a  = a

--    replaceStatus eStat e = case e of
  --                              Status _ -> Status eStat
    --                            otherwise -> e

    conformHead :: Attribute -> [ImgColor]
    conformHead (Status estat) = do
        c <- whichColor estat
        res <- (conformHeadColor c (cycle availableColors))
        return res

    conformHeadColor :: ImgColor -> [ImgColor] -> [ImgColor]
    conformHeadColor headColor l = dropWhile (\x -> (x/= headColor)) l 
    
    data EStatus = Initial | Valid | Invalid |  Verified | Proposed | Falsified
        deriving(Read,Show,Ord,Eq)

    
    data Attribute = Label String | Author String | ID Int | Status EStatus 
        deriving(Read,Show)

    statFromMeta :: Meta -> Attribute
    statFromMeta (Meta atts) = statFromMeta' atts where
        statFromMeta' meta@((Status x):xs)  =  Status x
        statFromMeta' meta@(_:xs)  =  statFromMeta' xs
 
    data Meta = NoMeta | Meta [Attribute]
        deriving(Read)

    instance Show Meta where
        show NoMeta = ""
        --show (Meta l) = "{" ++ show l ++ "}"
        show (Meta l) = show l

    data Text = Text String
       deriving(Read, Show)
    data Descr = Descr String
       deriving(Read, Show)

    data Term = Term String
       deriving(Read, Show)

    data ArgContent = 
        Thesis Text 
        | CounterExample Text 
        | Logic Text
        | Premise Text
        | Predicate Text
        | Rationale Text 
        | Definition  Term  Descr 
        deriving(Read)
    
    availableLettersDE :: [ImgLetter]
    availableLettersDE = [T,D,G,S,P,A,B]

    availableLettersEN :: [ImgLetter]
    availableLettersEN = [T,D,C,L,P,A,R] 

    -- Dialogue specific
    data Name = Name String 
        deriving(Read, Show)

    data SpeakerID = SPID Int 
        deriving(Read, Show)

    data Speaker = Speaker Name
        deriving(Read, Show)

    data SpeakerList = SpeakerList [(SpeakerID,Speaker)]
        deriving(Read, Show)

    data Speech = Speech Attribute [Text]
        deriving(Read, Show)

    data Dialogue = Dialogue Attribute [Speech]
        deriving(Read, Show)

    data Header = ArgHeader Meta ArgContent
        | DlgHeader Dialogue
        deriving(Read)
     
    instance Show Header where
--        show (ArgHeader a b) = show b ++ show a
        show (ArgHeader m (Thesis (Text s)))                  = "THESIS" ++ show m ++ ": " ++ s
        show (ArgHeader m (Definition (Term t) (Descr d)))    = "DEFINE" ++ show m ++ ": " ++ t ++ " AS " ++ d
        show (ArgHeader m (CounterExample (Text s)))          = "COUNTEREXAMPLE" ++ show m ++ ": " ++ s
        show (ArgHeader m (Logic (Text s)))                   = "LOGIC" ++ show m ++ ": " ++ s
        show (ArgHeader m (Premise (Text s)))                 = "PREMISE" ++ show m ++ ": " ++ s
        show (ArgHeader m (Predicate (Text s)))               = "PREDICATE" ++ show m ++ ": " ++ s
        show (ArgHeader m (Rationale (Text s)))               = "RATIONALE" ++ show m ++ ": " ++ s

        show (DlgHeader d) = show d 
   -- End Dialogue spcific part
    data MoveDirection = Down | Up
        deriving (Show)

    -- Content is arbitrary Argumentation components
--    data Content = Content [ ArgComp ] --( Meta )
  --      deriving(Read, Show)
    -- Argumentation component is Header + Content
    type ArgCompForest = [ArgComp] 
    data ArgComp = ArgComp Header ArgCompForest
         deriving(Read,Show)
-- STATE Control
-- IOREfs and MVARs suggested
    type ArgElemId = Int

--    type ArgPair a b = (a,b)
    type ArgPairForest a b = [Tree (a, b)]
    type ArgPairTree a b = Tree (a,b)
    
    test_id_tree = (Node ("test1",1) [ (Node ("test11",11) [(Node ("test111",111) []) ]), (Node ("test12",12) []) ]) :: Tree (String,ArgElemId)
    
    removeById :: Eq b => [Tree (a,b)] -> b -> [Tree (a,b)]
    removeById [] _ = []
    removeById ((Node (v,nid) children):aps) id        
        | nid == id     = aps                                                             -- forget this node
        | otherwise     = (Node (v,nid) (removeById children id) ):(removeById aps id)    -- take this one and remove from rest if rest present

    moveById :: Eq b => Tree (a,b) -> b -> MoveDirection  ->  Tree (a,b)
    --moveById _ _ _ = []
    moveById tree@(Node (v,nid) children) id direction
        | isIn id children = Node (v,nid) (moveChildInListById children id direction)
        | otherwise     = (Node (v,nid) (moveInListById children id direction)) -- step down 
              
    moveChildInListById [] _ _ = []
    moveChildInListById (x:[]) _  _ = [x]
    moveChildInListById (x:xs) id Down 
        | compareNodeId x id  = (head xs):x:(tail xs)
        | otherwise = x:(moveChildInListById xs id Down)
    moveChildInListById (y:x:xs) id Up 
        | compareNodeId x id  = (x:y:xs)
        | otherwise = y:x:(moveChildInListById xs id Up)
    
    moveInListById [] _ _ = []  
    moveInListById (c:ch) id direction = (moveById c id direction):(moveInListById ch id direction)

    isIn :: Eq b => b -> [Tree (a,b)] -> Bool
    isIn id [] = False
    isIn id (c:ch) = (compareNodeId c id) || (isIn id ch)
    
    compareNodeId (Node (v,nid) _) id = id == nid
    
    prependById :: Eq b => [Tree (a,b)] -> Tree (a,b) -> b -> [Tree (a,b)]
    prependById [] _ _ = []
    prependById (first@(Node (v,nid) children):aps) newNode id
        | nid == id      = newNode:first:aps
        | otherwise      = (Node (v,nid) (prependById children newNode id) ):(prependById aps newNode id)
 
    appendById :: Eq b => [Tree (a,b)] -> Tree (a,b)-> b -> [Tree (a,b)]
    appendById [] _ _ = []
    appendById (first@(Node (v,nid) children):aps) newNode id
        | nid == id      = first:newNode:aps
        | otherwise      = (Node (v,nid) (appendById children newNode id) ):(appendById aps newNode id)

    addById :: Eq b => [Tree (a,b)] -> Tree (a,b) -> b -> [Tree (a,b)]
    addById [] _ _ = []
    addById (first@(Node (v,nid) children):aps) newNode id
        | nid == id      = (adoptChild first newNode):aps
        | otherwise      = (Node (v,nid) (addById children newNode id)):(addById aps newNode id)

    getNodeById :: Eq b => Tree (a,b) -> b -> Maybe (Tree (a,b))
    getNodeById thisTree@(Node (v,nid) ch) id
        | nid == id = Just thisTree
        | otherwise = getNodeByIdFromList ch id
    
    getNodeByIdFromList :: Eq b => [Tree (a,b)] -> b -> Maybe (Tree (a,b))
    getNodeByIdFromList [] id = Nothing
    getNodeByIdFromList (x:xs) id = 
          case y of
            Nothing -> getNodeByIdFromList xs id                    
            Just t  -> y 
            where y = getNodeById x id 

    getChildrenById :: Eq b => [Tree (a, b)] -> b -> [Tree (a, b)]
    getChildrenById l id = 
            case maybeTree of
                Nothing -> []
                Just t  -> getChildren t
            where 
                getChildren (Node (v,nid) ch) = ch
                maybeTree = getNodeByIdFromList l id

    adoptChild :: ArgPairTree a b -> ArgPairTree a b-> ArgPairTree a b
    adoptChild (Node p children) newChild = Node p (children ++ [newChild])

    type OpCount = Int
    type ArgCompState = ([ArgComp],OpCount)
    initialState = ([],0) :: ArgCompState 

    add :: ArgComp -> State ArgCompState OpCount
    add argComp = do
        (curr,b) <- get
        put ([argComp],(b+1))
        return (b+1)
    
    replaceACList :: [ArgComp] -> State ArgCompState OpCount
    replaceACList l@(ac:acs) = do
        _ <- (add ac)
        _ <- replaceACList acs
        (curr,b) <- get
        return b
    replaceACList [] = do        
        (curr,b) <- get
        return b

--OUTPUT
    drawArgComp :: ArgComp -> [String]
    drawArgComp (ArgComp node children) = show node : drawSubTrees children
      where
        drawSubTrees [] = []
        drawSubTrees [t] =
        --    "|" : shift "`- " "   " (draw t)
            shift "   " "   " (drawArgComp t)
        drawSubTrees (t:ts) =
        --    " " : shift "   " "   " (draw t) ++ drawSubTrees ts
            shift "   " "   " (drawArgComp t) ++ drawSubTrees ts

        shift first other = zipWith (++) (first : repeat other)

    drawArgCompForest :: [ArgComp] -> IO () 
    drawArgCompForest list@(h:r) = do
        drawTreeOne h
        drawArgCompForest r
    drawArgCompForest [] = do
        return ()

    drawTreeOne :: ArgComp -> IO ()
    drawTreeOne theOne =  mapM_ putStrLn  (drawArgComp theOne)

    drawTreeOneToFile :: ArgComp -> Handle -> IO ()
    drawTreeOneToFile argComp outh = mapM_ (hPutStrLn outh) (drawArgComp argComp)

    drawTreeToFile :: [ArgComp] -> Handle -> IO ()    
    drawTreeToFile (h:r) outh = do
        drawTreeOneToFile h outh
        drawTreeToFile r outh
    drawTreeToFile [] _ = do
        return ()
    
    writeACToFile :: FilePath -> [ArgComp] -> IO ()
    writeACToFile filename argComps = do
            outh <- openFile filename WriteMode
            drawTreeToFile argComps outh
            hClose outh
     
{- 
    thesisNew :: String -> ArgComp
    thesisNew textStr        = ArgComp                 --               
                                (ArgHeader 
                                    NoMeta 
                                    (Thesis 
                                        (Text textStr)
                                    ) 
                                ) 
                                []

    defNew :: String -> String -> ArgComp
    defNew termStr descrStr  = ArgComp 
                                (ArgHeader 
                                    NoMeta 
                                    (Definition   
                                        (Term termStr)
                                        (Descr descrStr)
                                    )
                                ) 
                                [] 
-}
    mt1 = Meta [Label "defaultMeta", Author "its me"]
    --init_argumentation = Content Nothing [] NoMeta --init
    test_dialogue = DlgHeader $ Dialogue (Label "Test dialogue")
                       [Speech (Label "ID 1") [Text "Did you have a good day?"], 
                        Speech (Label "ID 2") [Text "OH, yeah good. Thanks. How are you?"]]

