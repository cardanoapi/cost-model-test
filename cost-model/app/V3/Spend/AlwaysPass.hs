{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.AlwaysPass where

import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Prelude

mkWrappedValidator :: BuiltinData -> BuiltinUnit
mkWrappedValidator ctx = check True

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
validator = $$(PlutusTx.compile [||mkWrappedValidator||])
