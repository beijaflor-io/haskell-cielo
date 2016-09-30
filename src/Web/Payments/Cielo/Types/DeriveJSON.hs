module Web.Payments.Cielo.Types.DeriveJSON where

import qualified Data.Aeson.TH              as Aeson
import           Data.Char
import           Language.Haskell.TH.Syntax

deriveJSON :: Name -> Q [Dec]
deriveJSON t = Aeson.deriveJSON
                 Aeson.defaultOptions
                   { Aeson.fieldLabelModifier = drop (length (nameBase t))
                   , Aeson.constructorTagModifier = drop (length (nameBase t))
                   }
                 t

uncapitalize :: String -> String
uncapitalize "" = ""
uncapitalize (c:cs) = toLower c : cs
