{- | 
The MenuCreator module is a mere helper module which provides menu creation as it grows out of the view code to be reused on different pages in consideration. 

-}

module Gtk.MenuCreator (
   createMenuBar,
   default_menuBarDescr 
) where

import Graphics.UI.Gtk

createMenuBar descr = do 
    bar <- menuBarNew
    mapM_ (createMenu bar) descr
    return bar
    where
        createMenu bar (name,items) = do 
            menu <- menuNew
            item <- menuItemNewWithLabelOrMnemonic name
            menuItemSetSubmenu item menu
            menuShellAppend bar item
            mapM_ (createMenuItem menu) items
        createMenuItem menu (name,action) = do 
            item <- menuItemNewWithLabelOrMnemonic name
            menuShellAppend menu item
            case action of
                 Just act -> onActivateLeaf item act
                 Nothing  -> onActivateLeaf item (return ())
        menuItemNewWithLabelOrMnemonic name
          | elem '_' name = menuItemNewWithMnemonic name
          | otherwise     = menuItemNewWithLabel name

default_menuBarDescr = 
      [ ("_File", [ ("Open", Nothing)
                  , ("Save", Nothing)
                  , ("_Quit", Just mainQuit)
                  ]
        )
      , ("Help",  [ ("_Help", Nothing)
                  ]
        )
      ]

--menuBar <- createMenuBar menuBarDescr
