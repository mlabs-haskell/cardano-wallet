{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- An isomorphism between 'InAnyCardanoEra' and 'EraValue'.
--

module Cardano.Wallet.Read.Eras.InAnyCardanoEra
    ( isoInAnyCardanoEra
    )
  where

import Prelude

import Cardano.Api
    ( CardanoEra (..), InAnyCardanoEra (InAnyCardanoEra), IsCardanoEra )
import Cardano.Wallet.Read.Eras.EraValue
    ( EraValue (..)
    , MkEraValue (..)
    , allegra
    , alonzo
    , babbage
    , byron
    , eraValueSerialize
    , mary
    , shelley
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( KnownEras )
import Data.Generics.Internal.VL
    ( Iso', build, iso )
import Generics.SOP
    ( K (..), NP (..), Proxy (Proxy) )
import Generics.SOP.Classes
import Generics.SOP.NP
    ( cmap_NP )
import Generics.SOP.NS
    ( ap_NS )

toInAnyCardanoEra :: EraValue f -> InAnyCardanoEra f
toInAnyCardanoEra (EraValue f)
    = fst . build eraValueSerialize . EraValue . ap_NS existentials $ f
    where
    cardanoEras :: NP CardanoEra KnownEras
    cardanoEras =
        ByronEra
            :* ShelleyEra
            :* AllegraEra
            :* MaryEra
            :* AlonzoEra
            :* BabbageEra
            :* Nil

    mkExistential
        :: IsCardanoEra era
        => CardanoEra era
        -> (f -.-> K (InAnyCardanoEra f)) era
    mkExistential e = Fn (K . InAnyCardanoEra e)

    existentials = cmap_NP (Proxy @IsCardanoEra) mkExistential cardanoEras

fromInAnyCardanoEra :: InAnyCardanoEra f -> EraValue f
fromInAnyCardanoEra (InAnyCardanoEra era x) = case era of
  ByronEra -> inject byron x
  ShelleyEra -> inject shelley x
  AllegraEra -> inject allegra x
  MaryEra -> inject mary x
  AlonzoEra -> inject alonzo x
  BabbageEra -> inject babbage x
  where
    inject :: MkEraValue f era -> f era -> EraValue f
    inject (MkEraValue p) = build p

-- | An isomorphism between 'InAnyCardanoEra' and 'EraValue'.
isoInAnyCardanoEra :: Iso' (EraValue f) (InAnyCardanoEra f)
isoInAnyCardanoEra = iso toInAnyCardanoEra fromInAnyCardanoEra
