{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Shelley.
module Cardano.Wallet.Read.Primitive.Tx (fromCardanoTx) where

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Cardano
  ( Tx (ByronTx),
  )
import qualified Cardano.Api.Shelley as Cardano
import Cardano.Wallet.Primitive.Types
  ( Certificate (..),
  )
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import Cardano.Wallet.Read.Primitive.Tx.Allegra
  ( fromAllegraTx,
  )
import Cardano.Wallet.Read.Primitive.Tx.Alonzo
  ( fromAlonzoTx,
  )
import Cardano.Wallet.Read.Primitive.Tx.Babbage
  ( fromBabbageTx,
  )
import Cardano.Wallet.Read.Primitive.Tx.Byron
  ( fromTxAux,
  )
import Cardano.Wallet.Read.Primitive.Tx.Mary
  ( fromMaryTx,
  )
import Cardano.Wallet.Read.Primitive.Tx.Shelley
  ( fromShelleyTx,
  )
import Cardano.Wallet.Transaction
  ( TokenMapWithScripts (..),
    ValidityIntervalExplicit (..),
    emptyTokenMapWithScripts,
  )
import Prelude

fromCardanoTx ::
  Cardano.Tx era ->
  ( W.Tx,
    TokenMapWithScripts,
    TokenMapWithScripts,
    [Certificate],
    Maybe ValidityIntervalExplicit
  )
fromCardanoTx = \case
  Cardano.ShelleyTx era tx -> case era of
    Cardano.ShelleyBasedEraShelley ->
      extract $ fromShelleyTx tx
    Cardano.ShelleyBasedEraAllegra ->
      extract $ fromAllegraTx tx
    Cardano.ShelleyBasedEraMary ->
      extract $ fromMaryTx tx
    Cardano.ShelleyBasedEraAlonzo ->
      extract $ fromAlonzoTx tx
    Cardano.ShelleyBasedEraBabbage ->
      extract $ fromBabbageTx tx
  Cardano.ByronTx tx ->
    ( fromTxAux tx,
      emptyTokenMapWithScripts,
      emptyTokenMapWithScripts,
      [],
      Nothing
    )
  where
    extract (tx, certs, mint, burn, validity) =
      (tx, mint, burn, certs, validity)
