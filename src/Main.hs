{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
{-|
Module      :  Main
Copyright   : 
License     : GPL2

Maintainer  : msjean@gmx.de
Stability   : experimental
Portability : POSIX

This is the main entry point for the ArguEdit.bin executable.
Further entry points such as for testing purposes are also meant to be added here.
-}
--
-- 
--
-----------------------------------------------------------------------------

module Main (
    main
) where


import ArguEdit (start)

import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
--import Test.QuickCheck.All (quickCheckAll)

-- Simple function to create a hello message.
--hello s = "Hello " ++ s

-- Tell QuickCheck that if you strip "Hello " from the start of
-- hello s you will be left with s (for any s).
--prop_hello s = stripPrefix "Hello " (hello s) == Just s

-- Hello World
exeMain = do
--    putStrLn (hello "World")
    ArguEdit.start

-- Entry point for unit tests.
--testMain = do
--    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
--    unless allPass exitFailure

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

