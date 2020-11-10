{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import Ivory.Language

[ivory| string struct UARTBuffer 256 |]

uartTypes :: Module
uartTypes = package "uartTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)
