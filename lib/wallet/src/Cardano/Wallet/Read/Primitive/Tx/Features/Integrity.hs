{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Features.Integrity
    ( integrity
    , txIntegrityF
    , txIntegrity
    , txIntegrityCardanoApi
    )

 where

import Prelude hiding
    ( (.) )

import Cardano.Ledger.Alonzo.Tx
    ( ScriptIntegrityHash )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.SafeHash
    ( SafeToHash (originalBytes) )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), EraValue, K (..), applyEraFun, extractEraValue )
import Cardano.Wallet.Read.Tx
    ( Tx )
import Cardano.Wallet.Read.Tx.Cardano
    ( fromCardanoApiTx )
import Cardano.Wallet.Read.Tx.Integrity
    ( Integrity (..), getEraIntegrity )
import Control.Category
    ( (.) )
import Data.Maybe.Strict
    ( StrictMaybe, strictMaybeToMaybe )

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Primitive.Types.Hash as W

integrity :: EraFun Integrity (K (Maybe (W.Hash "ScriptIntegrity")))
integrity = EraFun
    { byronFun = noIntegrity
    , shelleyFun = noIntegrity
    , allegraFun = noIntegrity
    , maryFun = noIntegrity
    , alonzoFun = yesIntegrity
    , babbageFun = yesIntegrity
    }
    where
        noIntegrity = const $ K Nothing
        yesIntegrity (Integrity es) = K $ getIntegrity es

getIntegrity
    :: StrictMaybe (ScriptIntegrityHash StandardCrypto)
    -> Maybe (W.Hash "ScriptIntegrity")
getIntegrity = strictMaybeToMaybe . fmap (W.Hash . originalBytes)

-- useful to cache this composition here
txIntegrityF :: EraFun Tx (K (Maybe (W.Hash "ScriptIntegrity")))
txIntegrityF = integrity . getEraIntegrity

-- this is a helper that reuses the cache
txIntegrity :: EraValue Tx -> Maybe (W.Hash "ScriptIntegrity")
txIntegrity = extractEraValue . applyEraFun txIntegrityF

txIntegrityCardanoApi :: Cardano.Tx era -> Maybe (W.Hash "ScriptIntegrity")
txIntegrityCardanoApi = txIntegrity . fromCardanoApiTx
