{-|
The ArguEdit module is the actual programm controler, which binds all other modules.
The Controler gets to react on user input coming from the ArgView module and passing it on to the parser following the Model.
Thus it can be regarded as Model View Controler architecture implementation in a loose sense of the term.

ArguEdit loads a glade file for the basic visualization which is added the different GTK representations of argument components like thesis, premise or definition.

The main container allows for loading from and saving to a file as well as other clickings.
Hover the status icons to reveal operations on the components. Clicking the status icons changes the status icon. 
-}
module ArguEdit (
    start
) where

--import Graphics.UI.Gtk
-- view imports (gtk)
import Graphics.UI.Gtk.Gdk.Keys as Keys
import Graphics.UI.Gtk.Selectors.FileChooserDialog
import Graphics.UI.Gtk.ActionMenuToolbar.Action
import Graphics.UI.Gtk.ActionMenuToolbar.ActionGroup

--import Graphics.UI.Gtk.Selectors.FileChooserDialog
import Graphics.UI.Gtk hiding (Meta) --todo replace with exacte import
--import Graphics.UI.Gtk.Selectors.FileChooserDialog
--import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Signals

-- Model Imports
--import qualified Text.Show.Pretty as Pr
import Data.Maybe
import Data.IORef
import Control.Monad.State
import Control.Concurrent (threadDelay)
import Data.Tree as DTree
import qualified Data.Map.Strict as Map

--local
import Specification.ArguModel as ArguModel
import Indentparser.ArguParser as ArguParser
import Gtk.MenuCreator as MenuC
import ArguView as AView

--Controler
type ArgViewTree = ArgPairTree ArgHeaderView ArgElemId
type ArgViewForest = [ArgViewTree]
type ArgViewState = (ArgViewTree,ArgElemId)
type IOArgData =  (Map.Map HBox ArgElemId,ArgViewForest,Int,ArgCompForest, VBox, Maybe ArgComp )

--Controler
mkVNode :: ArgHeaderView -> ArgElemId -> ArgViewForest -> ArgViewTree
mkVNode v id ch = Node (v, id) ch
test_view_tree = mkVNode EmptyView 0 []


-- Controler
toArgCompForest :: ArgViewForest -> IO ArgCompForest
toArgCompForest avforest = do 
    res <- mapM toArgCompTree avforest
    return res

toArgCompTree :: ArgViewTree -> IO ArgComp
toArgCompTree av@(Node (v,id) ch) = do
    h <- toHeader v
    ach <- toArgCompForest ch
    return $ ArgComp h ach 

toHeader :: ArgHeaderView -> IO Header
toHeader ahtv@(ArgHeaderThesisView common )      = do
    text <- getText $ tview  common
    meta <-  (getMeta common) 
    return $ ArgHeader (meta) $ Thesis $ Text text

toHeader ahdefv@(ArgHeaderDefView common specific)      = do
    definiendum <- getText $ tview  common
    definiens <- entryGetText $ term specific -- getDefiniens specific
    meta <-  (getMeta common) 
    return $    ArgHeader (meta) $ Definition (Term definiens) (Descr definiendum)

toHeader ahtv@(ArgHeaderCntrExmplView common )   = do 
    text <- getText $ tview  common
    meta <-  (getMeta common) 
    return $    ArgHeader (meta) $ CounterExample $ Text text 

toHeader ahtv@(ArgHeaderPremiseView common )     = do
    text <- getText $ tview  common
    meta <-  (getMeta common) 
    return $    ArgHeader (meta) $ Premise $ Text text

toHeader ahtv@(ArgHeaderPredicateView common )   = do
    text <- getText $ tview  common
    meta <-  (getMeta common) 
    return $    ArgHeader (meta) $ Predicate $ Text text

toHeader ahtv@(ArgHeaderRationaleView common )   = do
    text <- getText $ tview  common
    meta <-  (getMeta common) 
    return $   ArgHeader (meta) $ Rationale $ Text text

toHeader ahtv@(ArgHeaderLogicView common )       = do
    text <- getText $ tview  common
    meta <-  (getMeta common) 
    return $   ArgHeader meta $ Logic $ Text text


-- Controler
--drawArgViewForest :: ArgViewForest -> IO ()
drawArgViewForest avf = map drawArgViewTree avf

drawArgViewTree :: Tree (a,Int) -> [String]
drawArgViewTree (Node (v,nid) children) = ("(node," ++ show nid ++")") : drawSubTrees children
      where
        drawSubTrees [] = []
        drawSubTrees [t] =
            "|" : shift "`--" "   " (drawArgViewTree t)
        --    shift "   " "   " (drawArgViewTree t)
        drawSubTrees (t:ts) =
            "|" : shift "|--" "   " (drawArgViewTree t) ++ drawSubTrees ts
        --    shift "   " "   " (drawArgViewTree t) ++ drawSubTrees ts

        shift first other = zipWith (++) (first : repeat other)

-- Controler
menu_item_load_argumentation filename ioRefioArgData = do
    parseResult <- ArguParser.parseArgumentation filename
    case parseResult of
        Left error -> print error
        Right result -> do
            putStrLn $ "Parsed file input:"
            drawArgCompForest result
            putStrLn $ "Loading data into viewer"
            ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
            loadArguComps org_box ioRefioArgData 0 result
                          
            putStrLn $ "ArgViewForest:"
            let drawable = drawArgViewForest avf
            mapM_ (mapM_ putStrLn) (drawable)
            putStrLn ("id map:" ++ show idm)
            putStrLn ("lastId:" ++ show lastId)
            putStrLn ("fiacf" ++ show fiacf)
            --putStrLn "Putting raw file input into current state"
            --argCurrState <- readIORef ioRefArgCurrState
            --let (opcnt,nextState) = (runState (ArguModel.add (a)) argCurrState )
            --let (opcnt,nextState) = (runState (ArguModel.replaceACList result) argCurrState )
            --writeIORef ioRefArgCurrState (result,1)
            -- save accessable State
            --writeIORef ioRefArgCurrState nextState
            --putStrLn $ "State modified"
          
-- Controler       
--loadArguComps :: BoxClass box => box -> [ArgComp] -> IO ()
loadArguComps target_box ioRefioArgData parentId l= do
    mapM_ (\h -> (loadArguCompTopLevel target_box ioRefioArgData parentId h) ) l
--     argViewTree <- loadArguCompTopLevel target_box ioRefioArgData h
     

loadArguCompTopLevel target_box ioRefioArgData parentId ac = do
     loadArguComp target_box ioRefioArgData parentId ac 
     -- SEPERATOR
     {-sep <- hSeparatorNew
     widgetShow sep
     boxPackStart target_box sep PackNatural 0-}

--loadArguComp :: BoxClass box => box -> IORef IOArgData -> ArgElemId -> ArgComp -> IO ()

loadArguComp target_box ioRefioArgData parentId (ArgComp (ArgHeader (Meta atts) (Definition (Term term ) (Descr descr))) ch ) = do
    buildControlStructure2 target_box ioRefioArgData parentId (mkGtkDefinition) (Meta atts) term descr ch

loadArguComp target_box ioRefioArgData parentId (ArgComp (ArgHeader (Meta atts) (Logic (Text text))) ch ) = do
    buildControlStructure target_box ioRefioArgData parentId (mkGtkLogic) (Meta atts) text ch

loadArguComp target_box ioRefioArgData parentId (ArgComp (ArgHeader (Meta atts) (Premise (Text text))) ch ) = do
    buildControlStructure target_box ioRefioArgData parentId (mkGtkPremise) (Meta atts) text ch
{-
loadArguComp target_box ioRefioArgData parentId (ArgComp (ArgHeader (Meta atts) (Predicate (Text text))) ch ) = do
    buildControlStructure target_box ioRefioArgData parentId (mkGtkPredicate) (Meta atts) text ch
-}
loadArguComp target_box ioRefioArgData parentId (ArgComp (ArgHeader (Meta atts) (CounterExample (Text text))) ch ) = do
    buildControlStructure target_box ioRefioArgData parentId (mkGtkCounterExample) (Meta atts) text ch

loadArguComp target_box ioRefioArgData parentId (ArgComp (ArgHeader (Meta atts) (Thesis (Text text))) ch ) = do
    buildControlStructure target_box ioRefioArgData parentId (mkGtkThesis) (Meta atts) text ch
    
 -- consider this commented to provoke controled failure
loadArguComp _ _ _ ac = do
    putStrLn $ "unimplemented pattern" ++ show ac
    return ()


buildControlStructure target_box ioRefioArgData parentId mkGtkObject meta text ch = do
     -- PREPARE and LOAD GTK VIEW
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let thisId = (lastId +1)
    gtkO <- mkGtkObject target_box ioRefioArgData meta thisId text 
    let gtkViewer = commonParts gtkO
    let tox = (tbox gtkViewer)
    newIOArgData <- buildViewTree gtkO target_box ioArgData thisId parentId
    writeIORef ioRefioArgData newIOArgData
    -- LOAD IN CHILDREN
    loadArguComps tox ioRefioArgData thisId ch

buildControlStructure2 target_box ioRefioArgData parentId mkGtkObject meta first second ch = do
    -- PREPARE and LOAD GTK VIEW
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let thisId = (lastId +1)
    gtkO <- mkGtkObject target_box ioRefioArgData meta thisId first second
    let gtkViewer = commonParts gtkO
    let tox = (tbox gtkViewer)
    newIOArgData <- buildViewTree gtkO target_box ioArgData thisId parentId
    writeIORef ioRefioArgData newIOArgData
    -- LOAD IN CHILDREN
    loadArguComps tox ioRefioArgData thisId ch


buildViewTree gtkViewer target_box ioArgData thisId parentId= do
    -- BUILD VIEW STRUCTURE
    let its_row = (row (commonParts gtkViewer))
    --addToBox target_box its_row PackNatural
    addToBox target_box its_row PackGrow
    let (idm,avf,lastId,fiacf,org_box,clipboard) = ioArgData
    
    let newNode = (mkVNode gtkViewer thisId [])
    let navf = case parentId of
                    0 -> avf ++ [newNode] --no parent
                    _ -> addById avf newNode parentId 
    let nidm = Map.insert its_row thisId idm
    let newIOArgData = (nidm,navf,thisId,fiacf,org_box,clipboard)
    
    -- MAKE VISIBLE 
    --widgetShowAll its_row
    return newIOArgData

-- Controler

newArgumentation rows_wrapper  ioRefioArgData = do
        ioArgData@(idMap,argViewForest,lastId,fiacf,org_box,clipboard) 
            <- readIORef ioRefioArgData
        -- reset old stuff -- todo: allow multitabbing, with additional new arg  
        widgetDestroy org_box
--        let argViewForest = []

        -- make new
        newRows <- newCol
        let newIOArgData = initIOArgData newRows
        addToBox rows_wrapper newRows PackNatural
        writeIORef ioRefioArgData newIOArgData 
   

extractToArgComp ioRefioArgData = do
{-    putStrLn "extractToArgComp called (testing)"
    ioArgData@(idMap,argViewForest,lastId,fileInputArgCompForest) <- readIORef ioRefioArgData
    putStrLn "Retrieved file input of Argumentation"
    drawArgCompForest fileInputArgCompForest -}
    putStrLn "extractToArgComp called (testing)"
    putStrLn $ "ArgViewForest:"
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let drawable = drawArgViewForest avf
    mapM_ (mapM_ putStrLn) (drawable)
    

-- Controler
mkGtkThesis parent_box ioRefioArgData meta thisId textstring = do
    let stat = statFromMeta meta
    let himg = HeaderImage T (conformHead stat) viewIconSizeThesis
    common <- mkGtkTextBox parent_box ioRefioArgData meta thisId textstring himg -- "../pics/letters/png/letter_t_grey.png" 35
    return $ ArgHeaderThesisView common

mkGtkLogic parent_box ioRefioArgData meta thisId textstring = do
    let stat = statFromMeta meta
    let himg = HeaderImage S (conformHead stat) viewIconSizeDefault
    common <- mkGtkTextBox parent_box ioRefioArgData meta thisId textstring himg -- "../pics/letters/png/letter_l_grey.png" 30
    return $ ArgHeaderLogicView common 

mkGtkPremise parent_box ioRefioArgData meta thisId textstring = do
    let stat = statFromMeta meta
    let himg = HeaderImage P (conformHead stat) viewIconSizeDefault
    common <- mkGtkTextBox parent_box ioRefioArgData meta thisId textstring himg --"../pics/letters/png/letter_p_grey.png" 30
    return $ ArgHeaderPremiseView common

{-
mkGtkPredicate parent_box ioRefioArgData meta thisId textstring = do
    common <- mkGtkTextBox parent_box ioRefioArgData thisId textstring "../pics/letters/png/letter_a_grey.png" 30
    return $ ArgHeaderPremiseView common
-}
mkGtkCounterExample parent_box ioRefioArgData meta thisId textstring = do
    let stat = statFromMeta meta
    let himg = HeaderImage G (conformHead stat) viewIconSizeDefault
    common <- mkGtkTextBox parent_box ioRefioArgData meta thisId textstring himg --"../pics/letters/png/letter_g_grey.png" viewIconSizeDefault
    return $ ArgHeaderCntrExmplView common

mkGtkTextBox parent_box ioRefioArgData meta thisId textstring himg = do
    --(img_col,img) <- newHeaderImage img_filename size
    (th_box,children_box, textview ) <- newTextBox "Default label" textstring
    row <- newRow
    ioRefhimg <- newIORef himg
    ioRefMeta <- newIORef meta 
    let cParts = (GtkCommonParts ioRefhimg ioRefMeta parent_box row children_box textview) 
    button_col <- newControleArea ioRefioArgData thisId himg cParts
    addToBox row button_col PackNatural
    --widgetShowAll button
    boxPackStart row th_box PackGrow 3
    widgetShow row
    return $ cParts -- GtkCommonParts row th_box textview

newControleArea ioRefioArgData thisId himg cParts = do
    button <- buttonNew --WithLabel "click me"
    img <-  newImage (fileNameFromHeaderImage himg)  (imgSize himg)
    buttonSetImage button img
    buttonSetRelief button ReliefNone -- ReliefNormal ReliefNone ReliefHalf
    menubar <- createMenuBar $  menuBarDescrElement ioRefioArgData thisId --parentId
    menubar2 <- createMenuBar $  menuBarDescrNeu ioRefioArgData thisId --parentId

    -- pressed button
    button `on` buttonActivated $ do
    --    putStrLn "button clicked"
        nHimg <- getHimg cParts
        --putStrLn $ "New HeaderIamge:" ++ show nHimg
        let hColor = head (imgColors nHimg)
        nextEStatus <- whichEStatus hColor         
        (Meta atts) <- getMeta cParts
        let nAtts = updateListAttribute (Status nextEStatus) atts
        updateMeta cParts (Meta nAtts)
        nextImg <- nextImage cParts
        buttonSetImage button nextImg

    -- hovered image button
    onEnter button $ do
    --    putStrLn "button entered"
        mapM_ toggleVisibility [menubar,menubar2]
       
    button_col <- newCol
    button_row <- newRow
    addToBox button_row button PackNatural
    addToBox button_row menubar PackNatural 
    addToBox button_row menubar2 PackNatural 
    addToBox button_col button_row PackNatural --wrap 
    widgetShowAll button_row
    widgetShow button_col
    widgetHide menubar
    widgetHide menubar2
    return button_col


-- Controler
mkGtkDefinition parent_box ioRefioArgData meta thisId term textstring = do
    let stat = statFromMeta meta
    let himg = HeaderImage D (conformHead stat) viewIconSizeDefinition
    (def_box,termview,descrview) <- newDefBox term textstring
    row <- newRow
    ioRefhimg <- newIORef himg
    ioRefMeta <- newIORef meta 
    let cParts = (GtkCommonParts ioRefhimg ioRefMeta parent_box row def_box descrview) 
    button_col <- newControleArea ioRefioArgData thisId himg cParts
    addToBox row button_col PackNatural
    boxPackStart row def_box PackGrow 3
    widgetShow row
    widgetShowAll def_box
    return $ ArgHeaderDefView cParts (GtkDefSpecific termview)
-- View
-- PRINTING GTK OBJECTS
instance Show Box  where 
  show b = "defaultShow Box"

-- END OF GTK PRINTING

-- Controler
menu_item_response action = do
    name <- actionGetName action
    putStrLn ("Action Name: " ++ name)


--openFileDialog :: Window -> ABox -> IORef ArgCompState -> IO ()
openFileDialog title parentWindow ioRefioArgData = do
    dialog <- fileChooserDialogNew
       (Just $ title )             --dialog title
       (Just parentWindow)                     --the parent window
       FileChooserActionOpen                          --the kind of dialog we want
       [("gtk-open"   , ResponseAccept)
       ,("gtk-cancel" , ResponseCancel)]             --The buttons to display

    widgetShow dialog
    response <- dialogRun dialog
    case response of
       ResponseAccept -> do
          Just fileName <- fileChooserGetFilename dialog
          putStrLn $ "you selected the file" ++ show fileName

          menu_item_load_argumentation fileName ioRefioArgData 

       ResponseCancel -> putStrLn "dialog canceled"
       ResponseDeleteEvent -> putStrLn "dialog closed"
    widgetHide dialog


-- Controler
saveFileDialog title parentWindow ioRefioArgData = do
    dialog <- fileChooserDialogNew
       (Just $ title )             --dialog title
       (Just parentWindow)                     --the parent window
       FileChooserActionSave                          --the kind of dialog we want
       [("gtk-save"   , ResponseAccept)
       ,("gtk-cancel" , ResponseCancel)]             --The buttons to display

    widgetShow dialog
    response <- dialogRun dialog
    case response of
       ResponseAccept -> do
          Just fileName <- fileChooserGetFilename dialog
          putStrLn $ "you selected the file " ++ show fileName
            
          putStrLn $ "Available Data:"
          ioArgData@(idMap,argViewForest,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
          putStrLn $ show org_box
          argu_out <- toArgCompForest argViewForest
--          show (access ++ " Accesses")
          putStrLn "Retrieved file input of Argumentation"
          let argu_in = fiacf --fileInputArgCompForest
          putStrLn $ "\nSaving data to selected file...:"
          drawArgCompForest argu_out
          writeACToFile fileName argu_out
          putStrLn $ "\nSaved test data to selected file " ++ fileName 
            
         -- menu_item_save_db target_box fileName
       ResponseCancel -> putStrLn "dialog canceled"
       ResponseDeleteEvent -> putStrLn "dialog closed"
    widgetHide dialog




-- Controler
actionGroupCreate window parent_box target_box= do
--    let argInitialState =  ([],0) :: ArgCompState
    let ioArgData = initIOArgData target_box
    
--    ioRefArgCurrentState <- newIORef argInitialState    
    ioRefioArgData <- newIORef ioArgData

    newa <- actionNew "NEWA" "New"     (Just "Neue Argumentation") (AView.getStockIcon "New")
    opna <- actionNew "OPNA" "Open"    (Just "Öffne Argumentations-Datei") (AView.getStockIcon "Open")
    sava <- actionNew "SAVA" "Save"    (Just "Speichern") (AView.getStockIcon "Save")
    svaa <- actionNew "SVAA" "Save As" (Just "Speichern als .. ") (AView.getStockIcon "SaveAs" )
    exia <- actionNew "EXIA" "Exit"    (Just "Schließen") (AView.getStockIcon "Quit" )
    --cuta <- actionNew "CUTA" "Cut"     (Just "Ausschneiden") (AView.getStockIcon "Cut" ) 
    --copa <- actionNew "COPA" "Copy"    (Just "Kopieren") (AView.getStockIcon "Copy" )
    --psta <- actionNew "PSTA" "Paste"   (Just "einfügen") (AView.getStockIcon "Paste" )
    hlpa <- actionNew "HLPA" "Help"    (Just "Hilfe") (AView.getStockIcon "Help" )
    --cona <- actionNew "CONA" "EXECUTE some tests"  (Just "Testen") (AView.getStockIcon "Execute" ) 

    nth <- actionNew "NTH" "These"          (Just "These") (AView.getStockIcon "New")
    ndf <- actionNew "NDF" "Definition"     (Just "Definition") (AView.getStockIcon "New")
    --nrt <- actionNew "NRT" "Begründung"     (Just "Begründung") (AView.getStockIcon "New")
    nce <- actionNew "NCE" "Gegenbeispiel"  (Just "Gegenbeispiel") (AView.getStockIcon "New")
    npr <- actionNew "NPR" "Prämisse"       (Just "Prämisse") (AView.getStockIcon "New")
    nlg <- actionNew "NLG" "Schluss"        (Just "Schluss") (AView.getStockIcon "New")


    first_level <- newFirstLevelActions
    let second_level = [nth,ndf{-,nrt-},nce,npr,nlg,   newa,opna,sava,svaa{-,cuta,copa,psta,hlpa,cona-}]

    onActionActivate nth (newThesis ioRefioArgData 0)
    onActionActivate ndf (newDefinition ioRefioArgData 0)
    --onActionActivate nrt (newRational ioRefioArgData 0)
    onActionActivate nce (newCounterExample ioRefioArgData 0)
    onActionActivate npr (newPremise ioRefioArgData 0)
    onActionActivate nlg (newLogic ioRefioArgData 0)

    --Action Activate Events
    onActionActivate svaa (saveFileDialog "Select database file to initialize (*.txt)" window ioRefioArgData)
    onActionActivate opna (openFileDialog "Select database file to load (*.txt)"  window ioRefioArgData )
    --onActionActivate cona (extractToArgComp ioRefioArgData )
    onActionActivate newa (newArgumentation parent_box ioRefioArgData)
    onActionActivate exia (widgetDestroy window)
    --onActionActivate cona (menu_item_test_data target_box)
    --onActionActivate cona (menu_item_create_test_db)
    --onActionActivate psta (menu_item_load_argumentation target_box)

    --ActionGroup
    agr <- actionGroupNew "AGR"
    --Keyboard Controles
    actionGroupAddActionWithAccel agr exia (Just "<Control>q")
    actionGroupAddActionWithAccel agr svaa (Just "<Ctrl>-s")
    actionGroupAddActionWithAccel agr opna (Just "<Control>o")

    mapM_ (actionGroupAddAction agr) first_level
    mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing) second_level

    mapM_ prAct second_level
    mapM_ actionConnectAccelerator second_level
    mapM_ (\act -> actionSetSensitive act True) second_level
        --actionSetAccelPath opna "<Actions>/AGR/OPNA"
    
{- working python equivalent
         exit = gtk.ImageMenuItem(gtk.STOCK_QUIT, agr)
         key, mod = gtk.accelerator_parse("<Control>Q")
         exit.add_accelerator("activate", agr, key, mod, gtk.ACCEL_VISIBLE) 
         exit.connect("activate", gtk.main_quit)
-} 
    --actionGroupAddActionWithAccel agr cona (Just "<ctrl>e")

    return agr


-- Controler
newFirstLevelActions = do
    nes <- actionNew "NES" "Neu" Nothing Nothing
    fma <- actionNew "FMA" "Datei" Nothing Nothing
--    ema <- actionNew "EMA" "Bearbeiten" Nothing Nothing
    hma <- actionNew "HMA" "Hilfe" Nothing Nothing
  --  dev <- actionNew "DEV" "Entwicklung" Nothing Nothing
  --  return [nes,fma,ema,hma,dev]
    return [nes,fma,hma]



-- #########         START           #############

start= main

-- #########         MAIN           ##############
main = do
       viewMain (actionGroupCreate) 

--initIOArgData :: BoxClass box => box -> (Map.Map HBox ArgElemId, [ArgViewTree],Int,ArgCompForest, VBox )
initIOArgData target_box =
         ((Map.fromList [])  ::Map.Map HBox ArgElemId,  -- idMap 
         []                  ::[ArgViewTree],           -- ArgViewForest
         0                   ::Int,                     -- lastId
         []                  ::ArgCompForest,           -- raw argCompForest (fileInput)
         target_box,
         Nothing             ::Maybe ArgComp
         )
 
btnmenuBarSecLevelStrings = 
                  [
--                  , "aufwärts"    -- w
--                  , "abwärts"     -- s
                    "kopieren"
                  , "einfügen"
                  , "löschen"
                  , "Ein-/Ausklappen"
                  ]

menuBarDescrElement ioRefArgData pid = 
--            [("\xF0\x9D\x9C\x86",
--            [("€",
--            [("ƒ",
            [("λ",
                 [
                    ("aufwärts", Just (moveElement ioRefArgData pid Up) )    -- w
                  , ("abwärts", Just (moveElement ioRefArgData pid Down) )   -- s
                  , ("kopieren", Just (copyElement ioRefArgData pid) )
                  , ("einfügen", Just (pasteElement ioRefArgData pid) )
                  , ("löschen",  Just (removeElement ioRefArgData pid)  )
                  , ("Ein-/Ausklappen", Just (toggleHideShowChildren ioRefArgData pid) )
                  ]
             )
            ]


menuBarDescrNeu ioRefArgData pid =
    [("+", 
        [("These", Just (newThesis ioRefArgData pid)), 
         ("Definition", Just (newDefinition ioRefArgData pid)),
         --("Begründung", Just (newRational ioRefArgData pid)),
         ("Gegenbeispiel", Just (newCounterExample ioRefArgData pid)),
         ("Prämisse", Just (newPremise ioRefArgData pid)),
--         ("Predikat",Just (newPredicate ioRefArgData pid)),
         ("Schluss",Just (newLogic ioRefArgData pid))
        ]
      )
     ] 


-- View -> Controler

--instance Show (IO ())
--    show io = "ElementOperation"

-- Controle
newElement      :: IORef IOArgData -> ArgElemId -> IO ()
newElement ioRefioArgData thisId         = do 
    putStrLn ("addElem_fct:"++(show thisId)) 
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let maybeTree = getNodeByIdFromList avf thisId
    case maybeTree of
        Just (Node (v,nid) ch) -> do
           let target_box = tbox $ commonParts $ v
           loadArguComp target_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Thesis (Text "Neue These"))) [] )
        Nothing ->  do
           loadArguComp org_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Thesis (Text "Neue These"))) [] )

newThesis :: IORef IOArgData -> ArgElemId -> IO ()
newThesis ioRefioArgData thisId         = do
    putStrLn ("newThesis_fct:"++(show thisId)) 
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let maybeTree = getNodeByIdFromList avf thisId
    case maybeTree of
        Just (Node (v,nid) ch) -> do
           let target_box = tbox $ commonParts $ v
           loadArguComp target_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Thesis (Text "Neue These"))) [] )
        Nothing ->  do
           loadArguComp org_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Thesis (Text "Neue These"))) [] )


newDefinition :: IORef IOArgData -> ArgElemId -> IO ()
newDefinition ioRefioArgData thisId         = do 
    putStrLn ("addElem_fct:"++(show thisId)) 
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let maybeTree = getNodeByIdFromList avf thisId
    case maybeTree of
        Just (Node (v,nid) ch) -> do
           let target_box = tbox $ commonParts $ v
           loadArguComp target_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Definition (Term "Definiens") (Descr "Definiendum"))) [])
        Nothing ->  do
           loadArguComp org_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Definition (Term "Definiens") (Descr "Definiendum"))) [])



newLogic :: IORef IOArgData -> ArgElemId -> IO ()
newLogic ioRefioArgData thisId         = do 
    putStrLn ("addElem_fct:"++(show thisId)) 
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let maybeTree = getNodeByIdFromList avf thisId
    case maybeTree of
        Just (Node (v,nid) ch) -> do
           let target_box = tbox $ commonParts $ v
           loadArguComp target_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Logic (Text "Eine neuer Schluss"))) [] )
        Nothing ->  do
           loadArguComp org_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Premise (Text "Eine neue Prämisse"))) [] )

newPremise :: IORef IOArgData -> ArgElemId -> IO ()
newPremise ioRefioArgData thisId         = do 
    putStrLn ("addElem_fct:"++(show thisId)) 
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let maybeTree = getNodeByIdFromList avf thisId
    case maybeTree of
        Just (Node (v,nid) ch) -> do
           let target_box = tbox $ commonParts $ v
           loadArguComp target_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Premise (Text "Eine neue Prämisse"))) [] )
        Nothing ->  do
           loadArguComp org_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Premise (Text "Eine neue Prämisse"))) [] )

newPredicate :: IORef IOArgData -> ArgElemId -> IO ()
newPredicate ioRefioArgData thisId         = do 
    putStrLn ("addElem_fct:"++(show thisId)) 
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let maybeTree = getNodeByIdFromList avf thisId
    case maybeTree of
        Just (Node (v,nid) ch) -> do
           let target_box = tbox $ commonParts $ v
           loadArguComp target_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Predicate (Text "Eine neues Predikat"))) [] )
        Nothing ->  do
           loadArguComp org_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Predicate (Text "Eine neues Predikat"))) [] )

newCounterExample :: IORef IOArgData -> ArgElemId -> IO ()
newCounterExample ioRefioArgData thisId         = do 
    putStrLn ("addElem_fct:"++(show thisId)) 
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let maybeTree = getNodeByIdFromList avf thisId
    case maybeTree of
        Just (Node (v,nid) ch) -> do
           let target_box = tbox $ commonParts $ v
           loadArguComp target_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (CounterExample (Text "Eine neues Gegenbeispiel"))) [] )
        Nothing ->  do 
           loadArguComp org_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (CounterExample (Text "Eine neues Gegenbeispiel"))) [] )

newRational :: IORef IOArgData -> ArgElemId -> IO ()
newRational ioRefioArgData thisId         = do 
    putStrLn ("addElem_fct:"++(show thisId)) 
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let maybeTree = getNodeByIdFromList avf thisId
    case maybeTree of
        Just (Node (v,nid) ch) -> do
           let t_box = tbox $ commonParts $ v
           loadArguComp t_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Rationale (Text "Eine neue Begründunbg"))) [] )
        Nothing ->  do 
            loadArguComp org_box ioRefioArgData thisId (ArgComp (ArgHeader (Meta [Status Initial]) (Rationale (Text "Eine neue Begründunbg"))) [] )

removeElement   :: IORef IOArgData -> ArgElemId -> IO ()
removeElement ioRefioArgData thisId      = do 
    putStrLn ("removeElem_fct:"++(show thisId))
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let maybeTree = getNodeByIdFromList avf thisId
    case maybeTree of
        Just tree@(Node (v,nid) ch) -> do
--            return ()  -- ioData.avf -> removeById + elem.row.destroy
            let navf = removeById avf thisId
            newArgComp <- toArgCompTree tree
            --Copy the removed elem to clipboard            
            writeIORef ioRefioArgData (idm,navf,lastId,fiacf,org_box,Just newArgComp)  -- newIOArgData
            let drawable = drawArgViewForest navf
            putStrLn "Tree after removal:" 
            mapM_ (mapM_ putStrLn) (drawable)
            widgetDestroy $ row $ commonParts v
            
        Nothing ->  do
            return () 

moveElement     :: IORef IOArgData -> ArgElemId -> MoveDirection -> IO ()
moveElement ioRefioArgData thisId  direction     = do
    putStrLn ("move-"++ show direction ++"-Elem_fct:"++ (show thisId))    -- ioData.store internal clipboard <- avf.copyByID
    -- get ElemByI 
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let maybeNode = getNodeByIdFromList avf thisId 
    case maybeNode of
        Just tree@(Node (v,nid) ch) -> do
            --newArgComp <- toArgCompTree tree
            --putStrLn $ "moving:" 
            -- get box            
            putStrLn $ show v ++ " : " ++ show nid
            mapM_ putStrLn $ drawArgViewTree tree
            -- get parent
            let cParts = commonParts v
            let theParent = parent cParts
            let rowv =  row cParts
            pos <- Graphics.UI.Gtk.get theParent (boxChildPosition rowv)
            putStrLn $ "Position:" ++ show pos
            let dirToInt = case direction of
                            Up -> (-1)
                            Down -> (1)            
            let newPos = max (pos + (dirToInt)) 1 -- HACK : hack to prevent zeror Element (textbox) to be switched down
            putStrLn $ "New Position:" ++ show newPos
            boxReorderChild theParent rowv newPos
            
            -- synchronization with internal tree not yet implemented

            let navf = moveInListById avf thisId direction
           {- let drawable = drawArgViewForest navf
            putStrLn "Tree after moving:" 
            mapM_ (mapM_ putStrLn) (drawable)-}

            let newIOArgData = (idm,navf,lastId,fiacf,org_box,clipboard)
            writeIORef ioRefioArgData newIOArgData
        Nothing ->  do
            putStrLn "No elem found to move" --shouldn't be happening 

copyElement     :: IORef IOArgData -> ArgElemId -> IO ()
copyElement ioRefioArgData thisId        = do
    putStrLn ("copyElem_fct:"++(show thisId))    -- ioData.store internal clipboard <- avf.copyByID
    -- get ElemByI 
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let maybeNode = getNodeByIdFromList avf thisId 
    case maybeNode of
        Just tree@(Node (v,nid) ch) -> do
            newArgComp <- toArgCompTree tree
            putStrLn $ "copied:"
            mapM_ putStrLn (drawArgComp newArgComp)
            let newIOArgData = (idm,avf,lastId,fiacf,org_box,Just newArgComp) 
            writeIORef ioRefioArgData newIOArgData
        Nothing ->  do
            putStrLn "No elem found to copie" --shouldn't be happening 
        --add newArgComp from Elem to ioRefArgData
    -- cut: let lr = [x | x <- l, x /=3]

pasteElement     :: IORef IOArgData -> ArgElemId -> IO () 
pasteElement ioRefioArgData thisId       = do
    putStrLn ("pasteElem_fct:"++(show thisId))     -- ioData. ioRefioArgData thisId.adoptChild  (newFromClipboard newId)
    -- let viewElem = getById avf thisId
    -- get ArgComp from ioRefioArgData
    --   
    -- loadArguComp (tox viewElem) ioRefioArgData  pId  ArgComp
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    case clipboard of
        Just copiedArgComp -> do
            putStrLn "paste:"
            mapM_ putStrLn (drawArgComp copiedArgComp)
            let maybeNode = getNodeByIdFromList avf thisId 
            case maybeNode of                
                Just tree@(Node (v,nid) ch) -> do
                    let box = tbox (commonParts v)
                    loadArguComp box ioRefioArgData thisId copiedArgComp 
                Nothing -> do
                    putStrLn $ "No elem to paste to found"  -- shouldn't be happening

        Nothing -> putStrLn $ "Nothing to paste found" 


toggleHideShowChildren :: IORef IOArgData -> ArgElemId -> IO ()
toggleHideShowChildren ioRefioArgData thisId  = do 
    putStrLn ("toggleHideShow:"++(show thisId))
    ioArgData@(idm,avf,lastId,fiacf,org_box,clipboard) <- readIORef ioRefioArgData
    let children = getChildrenById avf thisId
  --  mapM_  children
    putStrLn "children:"
--    let drawable = drawArgViewForest children     
--    mapM_ (mapM_ putStrLn) (drawable)
    mapM_ toggleShowHideNode children
         -- ioData. id -> children widget visible 

toggleShowHideNode (Node (v,nid) ch) = do
         toggleShowHideView v

-- Controler

--globals
--constants


