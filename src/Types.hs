module Types (Orcid(..),
              DblpPersonId(..),
              DblpError(..)
              ) where

import Control.Exception (Exception)

data DblpError = ParseError String
               | OrcidNotFound String
               | ApiError String
               deriving (Eq, Show)

instance Exception DblpError

newtype Orcid = Orcid { unOrcid :: String } deriving (Show, Eq)

newtype DblpPersonId = DblpPersonId { unDblpPersonId :: String } deriving (Show, Eq)
