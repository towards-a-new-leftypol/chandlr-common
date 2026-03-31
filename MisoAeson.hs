-- Credit to georgefst for writing this (https://github.com/dmjio/miso/issues/1457#issuecomment-4054481403)

module Common.MisoAeson where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (first)
import Miso (ms)
import Miso.Aeson qualified
import Miso.JSON qualified

newtype MisoAeson a = MisoAeson a

instance (Aeson.ToJSON a) => Miso.JSON.ToJSON (MisoAeson a) where
    toJSON =
        Miso.Aeson.aesonToJSON
            . Aeson.toJSON
            . \(MisoAeson x) -> x
instance (Aeson.FromJSON a) => Miso.JSON.FromJSON (MisoAeson a) where
    parseJSON =
        fmap MisoAeson
            . Miso.JSON.Parser
            . first ms
            . flip Aeson.parseEither ()
            . const
            . Aeson.parseJSON
            . Miso.Aeson.jsonToAeson
