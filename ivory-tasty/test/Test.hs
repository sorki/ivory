{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}

module Main where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tasty

import Types

main :: IO ()
main = defaultMain $ ivoryTestGroup [uartTypes] []
  [ mkSuccess "smoke test" testSmoke
  , mkFailure "expected failure smoke test" testSmokeFailing
  , mkFailure "exptected failure on assert" testAssertFailing
  , setDeps (depend uartTypes) $ mkSuccess "test with dependencies" testDeps
  ]

testSmoke :: Def ('[] ':-> Sint32)
testSmoke = proc "main" $ body $ do
  ret 0

testSmokeFailing :: Def ('[] ':-> Sint32)
testSmokeFailing = proc "main" $ body $ do
  ret 1

testAssertFailing :: Def ('[] ':-> Sint32)
testAssertFailing = proc "main" $ body $ do
  assert false
  ret 0

testDeps :: Def ('[] ':-> Sint32)
testDeps = proc "main" $ body $ do
  (a :: Ref s UARTBuffer) <- local $ stringInit "yolo"
  lA <- deref (a ~> stringLengthL)
  assert (lA ==? 4)

  (b :: Ref s UARTBuffer) <- local $ stringInitFromHex "AABB"
  lB <- deref (b ~> stringLengthL)
  x0 <- deref (b ~> stringDataL ! 0)
  x1 <- deref (b ~> stringDataL ! 1)
  assert (lB ==? 2 .&& x0 ==? 0xAA .&& x1 ==? 0xBB)
  ret 0
