{- |
This is the module that contains the major viewing functionality for Arguedit.
That is, the argumentation components need a visual representation such as text boxes, 
clickable status icons and tree structured "contains" relation to have components belong to each other as wished.
-}

module ArguView where
import Graphics.UI.Gtk.MenuComboToolbar.ImageMenuItem
import Graphics.UI.Gtk.Abstract.Box as ABox
import Graphics.UI.Gtk.Multiline.TextBuffer
import Graphics.UI.Gtk.Multiline.TextView
import Graphics.UI.Gtk.Multiline.TextTag
import Graphics.UI.Gtk.Display.Image
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Layout.HBox
import Graphics.UI.Gtk.Layout.VBox
import Graphics.UI.Gtk.ActionMenuToolbar.UIManager
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.ActionMenuToolbar.Action
import Graphics.UI.Gtk.Windows.Window
import Graphics.UI.Gtk (get,mainQuit,ConnectId,mainGUI,initGUI)
import System.Glib.Attributes
--import Graphics.UI.Gtk.ActionMenuToolbar.ActionGroup
--import Graphics.UI.Gtk.Display.Image
import Graphics.UI.Gtk.Gdk.Pixbuf
--import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.RcStyle

-- Model imports
import Data.IORef
import qualified Data.Char as Char

--local
import Specification.ArguModel as ArguModel


--View
data GtkParts = 
        GtkCommonParts{ 
            ioRefHimg::IORef HeaderImage,
            ioRefMeta :: IORef Meta,
            parent::VBox,
            row::HBox,
            tbox::VBox,
            tview::TextView
        }|GtkDefSpecific{
            term::Entry
        }
--    deriving (Show)

instance Show GtkParts where
    show gtkParts = "default_gtkparts"

data HeaderImage=HeaderImage{
        imgLetter::ImgLetter,
        imgColors::[ImgColor],
        imgSize::Int
     }
--    deriving (Show)

instance Show HeaderImage where
    show (HeaderImage letter colors size) = "HeaderImage " ++ show letter ++ " "++ show (take 10 colors) ++" "++ show size

updateMeta cParts meta = do
    let refOnMeta = ioRefMeta cParts
    writeIORef refOnMeta meta

getMeta cParts = do
    let refOnMeta = ioRefMeta cParts
    meta <- readIORef refOnMeta
    return meta

getHimg cParts = do
   let refOnHimg = (ioRefHimg cParts)
   himg <- readIORef refOnHimg    
   return himg

nextImage cParts = do
   himg <- getHimg cParts
   let newHimg = HeaderImage (imgLetter himg) (tail (imgColors himg)) (imgSize himg)
   writeIORef (ioRefHimg cParts) newHimg
--   widgetShowAll img_col
   newImg <-  newImage (fileNameFromHeaderImage himg)  (imgSize himg)
   return newImg

{-
thisImage cParts = do
   himg <- readIORef ioRefHimg
--   let newHimg = HeaderImage (imgLetter himg) ( (imgColors himg)) (imgSize himg) 
   putStrLn ("this Image/new filename computed: "++"../pics/letters/png/letter_"++ show (imgLetter himg)  ++"_"++ show (head (imgColors himg)) ++ ".png") 

   (img_col,img) <- newHeaderImage ("../pics/letters/png/letter_"++"d"++"_red.png") (imgSize (himg cParts))
  -- writeIORef ioRefHimg newHimg
   widgetShowAll img_col 
   return img
-}

fileNameFromHeaderImage himg= 
   -- ("this Image/new filename computed: "
    ("../pics/letters/png/letter_"
    ++ toLower (show (imgLetter himg))
    ++"_"
    ++ toLower (show (head (imgColors himg)))
    ++ ".png") 

toLower str = map Char.toLower str

--View
data ArgHeaderView =
            EmptyView
            |ArgHeaderThesisView    {commonParts    ::GtkParts}
            |ArgHeaderCntrExmplView {commonParts    ::GtkParts}
            |ArgHeaderLogicView     {commonParts    ::GtkParts}
            |ArgHeaderPremiseView   {commonParts    ::GtkParts}
            |ArgHeaderPredicateView {commonParts    ::GtkParts}
            |ArgHeaderRationaleView {commonParts    ::GtkParts}
            |ArgHeaderDefView       {commonParts    ::GtkParts,
                                     specificParts  ::GtkParts}
    deriving (Show)

--instance Show (IORef HeaderImage) where
--    show ioR = "show_default ioRefHimg"

instance Show Entry where 
    show e = "show_default Entry"

instance Show TextView where
    show tv = "show_default TextView"

instance Show VBox where
    show vb = "show_default VBox"

instance Show HBox where
    show hb = "show_default HBox"

getText tv = do
    buf   <- textViewGetBuffer tv
    start <- textBufferGetStartIter buf 
    end   <- textBufferGetEndIter buf 
    text  <- textBufferGetText buf start end True
    return text

setText tv str = do
    buf <- textViewGetBuffer tv
    textBufferSetText buf str

-- View
newTextView :: String -> IO TextView
newTextView inittext  = do
    textview <- textViewNew
    tbuf <- newTextBuffer inittext
    textViewSetBuffer textview tbuf
    textViewSetJustification textview JustifyLeft
    textViewSetWrapMode textview WrapWord
    widgetShow textview
    return textview


newTextBuffer inittext = do
   tbuf <- textBufferNew Nothing
   textBufferSetText tbuf inittext
   --textBufferSetFontSize tbuf 28
   return tbuf

textBufferSetFontSize tbuf points = do
   font_size_tag <- textTagNew (Just "fontSizeTag")
   set font_size_tag [ textTagSizePoints := points, textTagFont := "Sans Italic 20" ] 
   start_iter <- textBufferGetStartIter tbuf
   end_iter <- textBufferGetEndIter tbuf
   textBufferApplyTag tbuf (font_size_tag) start_iter end_iter

newImage :: String -> Int -> IO Image
newImage filename size = do
    pbuf <- pixbufNewFromFileAtSize filename size size
    img  <- imageNewFromPixbuf pbuf
    return img

{-
newHeaderImage filename size = do
    img <- newImage filename size
    img_col <- newCol
    addToBox img_col img PackNatural
    return (img_col,img)
-}

--newRow :: Image -> TextView -> IO HBox
newRow = do
    row <- hBoxNew False 0
--    header_wrap <- newCol
--    boxPackStart header_wrap header PackNatural 0
--    boxPackStart row header_wrap PackNatural 0
--    boxPackStart row content PackGrow 0
    return row


newCol = do
    col <- vBoxNew False 0
--    (w,h) <- widgetGetSizeRequest col
--    widgetSetSizeRequest col w 50
    widgetShowAll col
    return col

getStockIcon iconName = 
    case iconName of
        "New"       -> Just stockNew
        "Open"      -> Just stockOpen
        "Save"      -> Just stockSave
        "SaveAs"    -> Just stockSaveAs
        "Quit"      -> Just stockQuit
        "Cut"       -> Just stockCut
        "Copy"      -> Just stockCopy
        "Paste"     -> Just stockPaste
        "Help"      -> Just stockHelp
        "Execute"   -> Just stockExecute


addToBox :: (BoxClass box, WidgetClass child) => box -> child -> Packing -> IO ()
addToBox box child packing = do
    boxPackStart box child packing 2

{-
addToBoxTL :: (BoxClass box, WidgetClass child) => box -> child -> Packing -> IO ()
addToBoxTL box child packing = do
    boxPackStart box child packing 0
    sep <- hSeparatorNew
    boxPackStart box sep PackNatural 0


addToBoxTLFromList target_box list =
     mapM_ (\ widget -> ( addToBoxTL target_box widget PackNatural )) list
-}

newTextBox label_string text_string = do
    th_box <- newCol
    children_box <- newCol
    label <- labelNew Nothing
    --labelSetMarkup label "<small>"++ text_string ++"</small>"
--    labelSetMarkup label ("<small>"++ label_string ++"</small>")
--    labelSetText label label_string
--    labelSetUseUnderline label True
--    labelSetJustify label JustifyRight
    textview <- newTextView text_string
    textViewSetLeftMargin textview 10
    boxPackStart children_box textview PackNatural 3
    boxPackStart th_box children_box PackNatural 3
--    labelWrap <- hBoxNew False 0
--    boxPackEnd th_box label PackNatural 0
    return (th_box, children_box, textview )


-- View
newDefBox term textstring = do
    term_edit <- entryNew
    entrySetText term_edit term
    entrySetActivatesDefault term_edit False
    entrySetHasFrame term_edit False
    descr_textview <- newTextView textstring
    def_box <- hBoxNew False 0
    term_edit_wrap <- vBoxNew False 0
    boxPackStart term_edit_wrap term_edit PackNatural 0
    boxPackStart def_box term_edit_wrap PackNatural 0
    boxPackStart def_box descr_textview PackGrow 4
    col <- newCol
    boxPackStart col def_box PackGrow 3
--    boxPackStart col def_box PackNatural 3
    return (col,term_edit,descr_textview)

-- View 
install_main_menu interface = do
    let (window, parent_box, target_box, agr ) = interface
    ui <- uiManagerNew
    uiManagerAddUiFromString ui uiDecl
    uiManagerInsertActionGroup ui agr 0

    maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
    let menubar = case maybeMenubar of
                        (Just x) -> x
                        Nothing -> error "Cannot get menubar from string."

    maybeToolbar <- uiManagerGetWidget ui "/ui/toolbar"
    let toolbar = case maybeToolbar of
                        (Just x) -> x
                        Nothing -> error "Cannot get toolbar from string."

    --add to gui
    boxPackStart parent_box menubar PackNatural 0
    boxPackStart parent_box toolbar PackNatural 0

-- View
prAct :: ActionClass self => self -> IO (ConnectId self)
prAct a = onActionActivate a $ do name <- actionGetName a
                                  accelpathIO <- actionGetAccelPath a
                                  let accelPath = case accelpathIO of
                                        Just x -> x
                                        Nothing -> "accelpath error"
                                  putStrLn ("Action Name: " ++ name ++ " ActionAccelpath: " ++ accelPath)

-- View
createMainGUI actionGroupCreate = do
    builder <- builderNew
    builderAddFromFile builder "./Gtk/arguedit_final.glade"
    main_window <- builderGetObject builder castToWindow "main_window"
    onDestroy main_window mainQuit
    rows_wrapper <- builderGetObject builder castToBox "rows"
    rows <- newCol
    main_box <- builderGetObject builder castToBox "main_box"
    addToBox rows_wrapper rows PackNatural 
--    agr <- newActions main_window main_box rows                 -- ACTIONS
    agr <- actionGroupCreate main_window rows_wrapper rows                 -- ACTIONS

    install_main_menu (main_window, main_box, rows, agr )       -- MENU
    widgetShowAll main_window

-- View
toggleShowHideView EmptyView = do return ()
toggleShowHideView v = do
         let therow = row (commonParts v)
         toggleVisibility therow
         return ()

-- View 
toggleVisibility :: WidgetClass a => a -> IO ()
toggleVisibility widget = do
    visibility <- Graphics.UI.Gtk.get widget widgetVisible
    case visibility of
        False -> widgetShow widget
        _     -> widgetHide widget

viewMain actionGroupCreate = do
    rcParse "./Gtk/gtkrc-2.0-arguedit"     -- STYLE
    initGUI                                 -- GTK INIT
    main_gui <- createMainGUI (actionGroupCreate)              -- GUI
    mainGUI                                 -- START


viewIconSizeThesis = 30 :: Int
viewIconSizeDefinition = 28 :: Int
viewIconSizeDefault = 25 :: Int
viewIconSizeMini = 5 :: Int
-- View

uiDecl=  "<ui>\
\           <menubar>\
\            <menu action=\"NES\">\
\              <menuitem action=\"NTH\" />\
\              <menuitem action=\"NDF\" />\
\              <menuitem action=\"NRT\" />\
\              <menuitem action=\"NCE\" />\
\              <menuitem action=\"NLG\" />\
\              <menuitem action=\"NPR\" />\
\            </menu>\
\            <menu action=\"FMA\">\
\              <menuitem action=\"NEWA\" />\
\              <menuitem action=\"OPNA\" />\
\              <menuitem action=\"SAVA\" />\
\              <menuitem action=\"SVAA\" />\
\              <separator />\
\              <menuitem action=\"EXIA\" />\
\            </menu>\
\           <menu action=\"EMA\">\
\              <menuitem action=\"CUTA\" />\
\              <menuitem action=\"COPA\" />\
\              <menuitem action=\"PSTA\" />\
\           </menu>\
\            <separator />\
\            <menu action=\"HMA\">\
\              <menuitem action=\"HLPA\" />\
\            </menu>\
\            <separator />\
\            <menu action=\"DEV\">\
\              <menuitem action=\"CONA\" />\
\            </menu>\
\           </menubar>\
\           <toolbar>\
\            <toolitem action=\"NEWA\" />\
\            <toolitem action=\"OPNA\" />\
\            <toolitem action=\"SAVA\" />\
\            <toolitem action=\"SVAA\" />\
\            <toolitem action=\"EXIA\" />\
\            <separator />\
\            <toolitem action=\"CUTA\" />\
\            <toolitem action=\"COPA\" />\
\            <toolitem action=\"PSTA\" />\
\            <separator />\
\            <toolitem action=\"HLPA\" />\
\            <separator />\
\            <toolitem action=\"CONA\" />\
\           </toolbar>\
\          </ui>"

