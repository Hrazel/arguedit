{- |
This is the core model of ArguEdit.
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
    {-| ImpLetter is the letter used to map an img file for each displayed img on the element buttons. -}
    data ImgLetter = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving (Eq,Show,Enum)
    {-| ImgColor is the color to mark an elements status. -}
    data ImgColor =  Black | Blue | DarkGreen | LightGreen |
                     Gold | Grey | Orange | Pink | Red | 
                     Violet deriving (Eq,Show,Enum,Ord)
    {-| The available colors is a subset of all the possible colors, which can be cicled through easily by making use of haskells list operations. -}
    availableColors :: [ImgColor]
    availableColors = [Grey,Blue,LightGreen,Red,Black]

    {-| The statColorMap is a bidireational map from ImgColor to an EStatus, meaning colors and status information refer to each other. Note: the *.arg files will not save a color but a status. -}
    statColorMap :: Bimap.Bimap ImgColor EStatus
    statColorMap = Bimap.fromList [(Grey,Initial),(Blue,Valid),(LightGreen,Verified),(Red,Falsified),(Black,Proposed)]

    {-| whichColor answers the question which color is mapped to this status passed to it.-}
    whichColor :: Monad m => EStatus -> m ImgColor
    whichColor stat = Bimap.lookupR stat statColorMap

    {-| whichEStatus answers the question which status is mapped to this color passed to it.-}
    whichEStatus :: Monad m => ImgColor -> m EStatus
    whichEStatus c = Bimap.lookup c statColorMap
   
    {-| updateListAttribute is a helper function to replace an Attribute which is somewhere in the list given.-} 
    updateListAttribute a l = map (replaceAttr a) l

    {-| replaceAttr returns being given two attributes the first one, but passed to the map function it acts as a replacement for the selected attribute (second argument). -}
    replaceAttr :: Attribute -> Attribute -> Attribute
    replaceAttr (Status e) (Status _) = Status e
    replaceAttr (Label s) (Label _) = Label s
    replaceAttr (Author a) (Author _) = Author a
    replaceAttr (ID id) (ID _) = ID id
    replaceAttr _ a  = a

--    replaceStatus eStat e = case e of
  --                              Status _ -> Status eStat
    --                            otherwise -> e

    {-| conformHead cycles through the availablecolors so that the head conforms the color that represents a given attributes.
    -}
    conformHead :: Attribute -> [ImgColor]
    conformHead (Status estat) = do
        c <- whichColor estat
        res <- (conformHeadColor c (cycle availableColors))
        return res

    {-| conformHeadColor conforms the head of a list of colors with a given color by dropping until a match is found. -}
    conformHeadColor :: ImgColor -> [ImgColor] -> [ImgColor]
    conformHeadColor headColor l = dropWhile (\x -> (x/= headColor)) l 
   
    {-| EStatus is the status of an element that is saved in the *.arg files.-} 
    data EStatus = Initial | Valid | Invalid |  Verified | Proposed | Falsified
        deriving(Read,Show,Ord,Eq)

    {-| Attribute allows elements to be attributed providing a Label, Author ID or Status, where only the status is used at the moment. -} 
    data Attribute = Label String | Author String | ID Int | Status EStatus 
        deriving(Read,Show)

    {-| statFromMeta retreives the status attribute from a list of attributes of a Meta objekt.-}
    statFromMeta :: Meta -> Attribute
    statFromMeta (Meta atts) = statFromMeta' atts where
        statFromMeta' meta@((Status x):xs)  =  Status x
        statFromMeta' meta@(_:xs)  =  statFromMeta' xs

    {-| Meta provides information about an element or indicates that no meta can be provided.-} 
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

    {-| ArgContent is the effetive content of an arguement element providing a thesis, counter example, a premise or the like.-}
    data ArgContent = 
        Thesis Text 
        | CounterExample Text 
        | Logic Text
        | Premise Text
        | Predicate Text
        | Rationale Text 
        | Definition  Term  Descr 
        deriving(Read)
   
    {-availableLettersDE provides a set of letters which represent german named element types where T refers to These, D to Definition, G to Gegenbeispiel, S to Schluss, P to prämisse, A to Attribut/Predikat, B to Begründung. It yet a preparation for langauge support.-} 
    availableLettersDE :: [ImgLetter]
    availableLettersDE = [T,D,G,S,P,A,B]

    {-availableLettersEN provides a set of letters which represent element types where T refers to thesis, D to definition, C to counter example, L to logical conclusion, P to premise, A to attribute/predicate, R to Rationale. It yet a preparation for langauge support.-} 
    availableLettersEN :: [ImgLetter]
    availableLettersEN = [T,D,C,L,P,A,R] 

    -- Dialogue specific
    {-| Preparation for dialogue integration. -}
    data Name = Name String 
        deriving(Read, Show)

    {-| Preparation for dialogue integration. -}
    data SpeakerID = SPID Int 
        deriving(Read, Show)

    {-| Preparation for dialogue integration. -}
    data Speaker = Speaker Name
        deriving(Read, Show)

    {-| Preparation for dialogue integration. -}
    data SpeakerList = SpeakerList [(SpeakerID,Speaker)]
        deriving(Read, Show)

    {-| Preparation for dialogue integration. -}
    data Speech = Speech Attribute [Text]
        deriving(Read, Show)

    {-| Preparation for dialogue integration. -}
    data Dialogue = Dialogue Attribute [Speech]
        deriving(Read, Show)

    {-| Preparation for dialogue integration . -}
    data Header = ArgHeader Meta ArgContent
        | DlgHeader Dialogue
        deriving(Read)
    
    {-| Printing the header of an argument element having meta information and effective content. It is realised as an instance of the Show function and is also used ouput to a *.arg file.-} 
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

    {-| Elements may be moved up and down.-}
    data MoveDirection = Down | Up
        deriving (Show)

    -- Content is arbitrary Argumentation components
--    data Content = Content [ ArgComp ] --( Meta )
  --      deriving(Read, Show)
    -- Argumentation component is Header + Content
    type ArgCompForest = [ArgComp] 
    {-| An Argcomp refering to argument component is a tree structure that represents an element of argumentation with all its belongings.-}
    data ArgComp = ArgComp Header ArgCompForest
         deriving(Read,Show)
-- STATE Control
-- IOREfs and MVARs suggested
    {-| Each element gets an id to identify a node.-}
    type ArgElemId = Int

--    type ArgPair a b = (a,b)
    type ArgPairForest a b = [Tree (a, b)]
    {-| ArgPairTree is a Tree of arbitrary types a and b supposed to be used as node + id. -}
    type ArgPairTree a b = Tree (a,b)
    
    test_id_tree = (Node ("test1",1) [ (Node ("test11",11) [(Node ("test111",111) []) ]), (Node ("test12",12) []) ]) :: Tree (String,ArgElemId)
   
    {-| Remove a node by its id.-} 
    removeById :: Eq b => [Tree (a,b)] -> b -> [Tree (a,b)]
    removeById [] _ = []
    removeById ((Node (v,nid) children):aps) id        
        | nid == id     = aps                                                             -- forget this node
        | otherwise     = (Node (v,nid) (removeById children id) ):(removeById aps id)    -- take this one and remove from rest if rest present

    {-| Move a node identified by its id up or down.-} 
    moveById :: Eq b => Tree (a,b) -> b -> MoveDirection  ->  Tree (a,b)
    --moveById _ _ _ = []
    moveById tree@(Node (v,nid) children) id direction
        | isIn id children = Node (v,nid) (moveChildInListById children id direction)
        | otherwise     = (Node (v,nid) (moveInListById children id direction)) -- step down 
    {-| Move a child of an element up or down. -} 
    moveChildInListById [] _ _ = []
    moveChildInListById (x:[]) _  _ = [x]
    moveChildInListById (x:xs) id Down 
        | compareNodeId x id  = (head xs):x:(tail xs)
        | otherwise = x:(moveChildInListById xs id Down)
    moveChildInListById (y:x:xs) id Up 
        | compareNodeId x id  = (x:y:xs)
        | otherwise = y:x:(moveChildInListById xs id Up)

    {-| Move around a node inside of a list -} 
    moveInListById [] _ _ = []  
    moveInListById (c:ch) id direction = (moveById c id direction):(moveInListById ch id direction)

    {-| Test if a node's id can be found in this tree. -}
    isIn :: Eq b => b -> [Tree (a,b)] -> Bool
    isIn id [] = False
    isIn id (c:ch) = (compareNodeId c id) || (isIn id ch)
    
    compareNodeId (Node (v,nid) _) id = id == nid
   
    {-| Add an Element just before another one identified by id. -} 
    prependById :: Eq b => [Tree (a,b)] -> Tree (a,b) -> b -> [Tree (a,b)]
    prependById [] _ _ = []
    prependById (first@(Node (v,nid) children):aps) newNode id
        | nid == id      = newNode:first:aps
        | otherwise      = (Node (v,nid) (prependById children newNode id) ):(prependById aps newNode id)
 
    {-| Add an Element right after another one identified by id. -} 
    appendById :: Eq b => [Tree (a,b)] -> Tree (a,b)-> b -> [Tree (a,b)]
    appendById [] _ _ = []
    appendById (first@(Node (v,nid) children):aps) newNode id
        | nid == id      = first:newNode:aps
        | otherwise      = (Node (v,nid) (appendById children newNode id) ):(appendById aps newNode id)

    {-| Add a new node to an identified node's children. -} 
    addById :: Eq b => [Tree (a,b)] -> Tree (a,b) -> b -> [Tree (a,b)]
    addById [] _ _ = []
    addById (first@(Node (v,nid) children):aps) newNode id
        | nid == id      = (adoptChild first newNode):aps
        | otherwise      = (Node (v,nid) (addById children newNode id)):(addById aps newNode id)

    {-| Get the node inside of a tree by its id.-}
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

    {-| Getter for the children of an identified node.-}
    getChildrenById :: Eq b => [Tree (a, b)] -> b -> [Tree (a, b)]
    getChildrenById l id = 
            case maybeTree of
                Nothing -> []
                Just t  -> getChildren t
            where 
                getChildren (Node (v,nid) ch) = ch
                maybeTree = getNodeByIdFromList l id

    {-|Adopt a child by appending it to a tree's chidlren.-}
    adoptChild :: ArgPairTree a b -> ArgPairTree a b-> ArgPairTree a b
    adoptChild (Node p children) newChild = Node p (children ++ [newChild])

    type OpCount = Int
    {-|Content of a state variable containing the state of the argumentation as a list of argument trees and counter to provide unique ids.-}
    type ArgCompState = ([ArgComp],OpCount)
    initialState = ([],0) :: ArgCompState 

    {-| Adding an increment to the running argumentation realized as stack inside the state monad.-}
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
    {-| Translate an argumentation to list of strings, that acts as output to be displayed on the screen or redirected to a file. -}
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
   
    {-Write an argumentation to a file.-} 
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
    {-| Just testing variable to play with. -}
    mt1 = Meta [Label "defaultMeta", Author "its me"]
    --init_argumentation = Content Nothing [] NoMeta --init
    {-| Just testing variable to play with. -}
    test_dialogue = DlgHeader $ Dialogue (Label "Test dialogue")
                       [Speech (Label "ID 1") [Text "Did you have a good day?"], 
                        Speech (Label "ID 2") [Text "OH, yeah good. Thanks. How are you?"]]

