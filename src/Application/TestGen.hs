{-# LANGUAGE OverloadedStrings #-}
module Application.TestGen where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Monad (mapM_)

import Domain.ProjectContext
import Domain.StructureTree
import Application.LLMGeneration
import Infrastructure.LLM.Ollama

------------------------------------------------------------------------------------------------------

testFilepath :: StructureNode -> FilePath
testFilepath tree = (getProjectname tree) </> "test"
    where
        getProjectname (ProjectNode {name = projectname}) = T.unpack projectname

acceptanceFilepath :: StructureNode -> FilePath
acceptanceFilepath tree = testFilepath tree </> "acceptanceSpec.hs"

genAcceptance :: ProjectContext -> StructureNode -> IO (FilePath)
genAcceptance context tree = do
    exist <- doesFileExist $ acceptanceFilepath tree
    if exist
        -- if acceptance test already exists
        then do
            putStrLn "An acceptance test is already exist."
            putStrLn "[U]pdate [K]eep [R]egenerate"
            usrInput <- getLine
            case usrInput of
                "U" -> updateAcceptance   context tree
                "K" -> return           $ acceptanceFilepath tree
                "R" -> newAcceptance      context tree
                _   -> genAcceptance      context tree
        -- if structure file isn't exists
        else do
            newAcceptance context tree

newAcceptance :: ProjectContext -> StructureNode -> IO (FilePath)
newAcceptance context tree = do
    putStrLn "Generating acceptance test ..."
    -- render prompt 
    let wholePrompt = T.replace "{{acceptance_tests}}" acceptance aPrompt
    -- generation
    llmGenerateAndSave (acceptanceFilepath tree) wholePrompt ollama (model aSpec)
    return $ acceptanceFilepath tree

    where
        aSpec      = spec context
        acceptance = acceptanceTest aSpec
        cPrompt    = prom context
        aPrompt    = pAcceptanceTest cPrompt

updateAcceptance :: ProjectContext -> StructureNode -> IO (FilePath)
updateAcceptance context tree = do
    putStrLn "Updating acceptance test ..."
    -- render prompt (load current acceptance test)
    currentFile <- TIO.readFile $ acceptanceFilepath tree
    let wholePrompt = T.replace "{{acceptance_tests}}" acceptance $ T.replace "{{current_testcode}}" currentFile aPrompt
    -- generation
    llmGenerateAndSave (acceptanceFilepath tree) wholePrompt ollama (model aSpec)
    return $ acceptanceFilepath tree

    where
        aSpec      = spec context
        acceptance = acceptanceTest aSpec
        cPrompt    = prom context
        aPrompt    = pAcceptanceTestRe cPrompt

------------------------------------------------------------------------------------------------------

