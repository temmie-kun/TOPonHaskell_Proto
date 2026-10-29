module Domain.StructureTree
  ( StructureNode(..)
  )
  where

import Data.Text (Text)

-- こっちのほうがドメインの制約を型で表現できていていいらしい
data StructureNode
    = ProjectNode
        { name        :: Text
        , description :: Text
        , children    :: [StructureNode]
        }
    | DirectoryNode
        { name        :: Text
        , description :: Text
        , children    :: [StructureNode]
        }
    | ModuleNode
        { name         :: Text
        , description  :: Text
        , children     :: [StructureNode]
        }
    | TypeNode
        { name           :: Text
        , description    :: Text
        }
    | MethodNode
        { name          :: Text
        , description   :: Text
        , typeSignature :: Text
        }
    deriving (Show)

{-
An expected structureTree (in YAML):

type: Project
name: ...
description: ...
children:

  - type: Directory
    name: app
    description: ...
    children:

      - type: Module
        name: Main
        description: ...
        children:

          - type: Type
            name: ...
            description: ...
          
          - type: Method
            name: ...
            description: ...
            typeSignature: ...

  - type: Directory
    name: src
    description: ...
    children:

      - type: Module
        name: ...
        description: ...
        children:
        
          - type: Type
            name: ...
            description: ...
          
          - type: Method
            name: ...
            description: ...
            typeSignature: ...
-}