{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import           Control.Applicative -- for GHC 7.8 compat
import           System.Environment (getArgs)
import           Module (JSON, liftNewStateT, runJSONParser, inside, earlyExit, Mytype(mytypeValue,mytypeValue2), Mytype2(mytype2Value), Notification(..), exitWith)
import qualified Data.List.NonEmpty as NE

import Debug.Trace

main :: IO ()
main = do
  print (runJSONParser (liftNewStateT (error "unused state") fun1))

fun1 :: JSON Mytype Float
fun1 =
  do
     ints <- liftNewStateT undefined fun2

     _ <- traceShow "before eval" $ error (show ints)

     undefined

fun2 :: JSON Mytype Int
fun2 = do
  x <- $(inside 'mytypeValue2) ($(inside 'mytype2Value) (exitWith Notification)) -- segfaults
  return x

  -- x <- $(inside 'mytypeValue2) (exitWith Notification)                      -- going only until depth 1: doesn't segfault
  -- return x

  -- $(inside 'mytypeValue2) ($(inside 'mytype2Value) (exitWith Notification)) -- not using do notation: doesn't segfault
