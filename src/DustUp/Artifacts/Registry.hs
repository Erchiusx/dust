module DustUp.Artifacts.Registry where

import Data.Text (Text)
import Data.Text qualified as Text
import DustUp.Artifacts.Column1.Core001 qualified as Core001
import DustUp.Artifacts.Column1.Core004 qualified as Core004
import DustUp.Artifacts.Column2.Core002 qualified as Core002
import DustUp.Artifacts.Column2.Core008 qualified as Core008
import DustUp.Artifacts.Column3.Core003 qualified as Core003
import DustUp.Artifacts.Column3.Core006 qualified as Core006
import DustUp.Artifacts.Type
import DustUp.Type

columnOneDefinitions :: [Artifact'Definition Column'One]
columnOneDefinitions =
  [ Core001.definition
  , Core004.definition
  ]

columnTwoDefinitions :: [Artifact'Definition Column'Two]
columnTwoDefinitions =
  [ Core002.definition
  , Core008.definition
  ]

columnThreeDefinitions :: [Artifact'Definition Column'Three]
columnThreeDefinitions =
  [ Core003.definition
  , Core006.definition
  ]

lookupColumnOne
  :: Card'ID
  -> Maybe (Artifact'Definition Column'One)
lookupColumnOne = lookupDefinition columnOneDefinitions

lookupColumnTwo
  :: Card'ID
  -> Maybe (Artifact'Definition Column'Two)
lookupColumnTwo = lookupDefinition columnTwoDefinitions

lookupColumnThree
  :: Card'ID
  -> Maybe (Artifact'Definition Column'Three)
lookupColumnThree = lookupDefinition columnThreeDefinitions

lookupDefinition
  :: [Artifact'Definition c]
  -> Card'ID
  -> Maybe (Artifact'Definition c)
lookupDefinition definitions wanted =
  case filter ((== wanted) . (.card'id)) definitions of
    definition : _ -> Just definition
    [] -> Nothing

cardLabel :: Card'ID -> Text
cardLabel cardID =
  case cardID.source of
    Core -> Text.pack "C" <> cardID.number
    Alternate -> Text.pack "A" <> cardID.number
