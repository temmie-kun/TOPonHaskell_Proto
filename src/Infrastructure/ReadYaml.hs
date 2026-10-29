{-# LANGUAGE OverloadedStrings #-}
module Infrastructure.ReadYaml where
-- 「Yamlを読んで型にパースする」責務
-- {{project_name}}などのプレースホルダーは，各generate関数内で解決したい

import Data.Text (Text)
import Data.Aeson(FromJSON(..),withObject,(.:),(.:?),(.!=))
import Data.Yaml(decodeFileEither)
import Control.Monad(mapM)

import Domain.ProjectContext
import Domain.StructureTree

------------------------------------------------------------------------------------------------------

-- read specification yaml
readSpec :: FilePath -> IO (Either String Specs)
readSpec path = do
    result <- decodeFileEither path
    case result of
        Left  err -> pure $ Left  $ show err
        Right spc -> pure $ Right $ spc

instance FromJSON Specs where
    parseJSON = withObject "Specs" $ \o ->
        Specs <$> o .: "model"
              <*> o .: "description"
              <*> o .: "acceptance_tests"

------------------------------------------------------------------------------------------------------

-- read prompt yaml
readProm :: FilePath -> IO (Either String Prompts)
readProm path = do
    result <- decodeFileEither path
    case result of
        Left  err -> pure $ Left  $ show err
        Right prm -> pure $ Right $ prm

instance FromJSON Prompts where
    parseJSON = withObject "Prompts" $ \o ->
        Prompts <$> o .: "structure_prompt"
                <*> o .: "structure_prompt_Re"
                <*> o .: "acceptanceTest_prompt"
                <*> o .: "acceptanceTest_prompt_Re"
                <*> o .: "moduleTest_prompt"
                <*> o .: "moduleTest_prompt_Re"
                <*> o .: "main_Module_Implementation_Prompt"
                <*> o .: "main_Module_Implementation_Prompt_Re"
                <*> o .: "module_Implementation_Prompt"
                <*> o .: "module_Implementation_Prompt_Re"

------------------------------------------------------------------------------------------------------

-- read structure yaml
readStructure :: FilePath -> IO (Either String StructureNode)
readStructure path = do
    result <- decodeFileEither path
    case result of
        Left err ->
            pure $ Left (show err)
        Right dto ->
            pure $ toDomain dto
    where
        toDomain :: NodeDto -> Either String StructureNode
        toDomain dto = do
            children <- mapM toDomain (childrenDto dto)
            case (nodetypeDto dto) of
                "Project"   -> Right $ ProjectNode   (nameDto dto) (descriptionDto dto) (children)
                "Directory" -> Right $ DirectoryNode (nameDto dto) (descriptionDto dto) (children)
                "Module"    -> Right $ ModuleNode    (nameDto dto) (descriptionDto dto) (children)
                "Type"      -> Right $ TypeNode      (nameDto dto) (descriptionDto dto)
                "Method"    -> Right $ MethodNode    (nameDto dto) (descriptionDto dto) (typeSignatureDto dto)
                other       -> Left  $ ("Unknown NodeType:" ++ (show other))

data NodeDto = NodeDto 
    { nodetypeDto      :: Text
    , nameDto          :: Text
    , descriptionDto   :: Text
    , typeSignatureDto :: Text
    , childrenDto      :: [NodeDto]
    }

instance FromJSON NodeDto where
    parseJSON = withObject "NodeDto" $ \o ->
        NodeDto <$> o .:  "type"
                <*> o .:  "name"
                <*> o .:? "description"   .!= ""
                <*> o .:? "typeSignature" .!= ""
                <*> o .:? "children"      .!= []