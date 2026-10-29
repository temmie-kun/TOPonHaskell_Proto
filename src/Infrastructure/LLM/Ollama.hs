{-# LANGUAGE OverloadedStrings #-}
module Infrastructure.LLM.Ollama where

import Control.Exception (try)
import Data.Aeson(FromJSON(..), Value, withObject, object, (.=), (.:),)
import Data.Text(Text)
import Network.HTTP.Req

import Domain.LLMRequest
import Application.LLMGeneration

ollama :: LLMRepository IO
ollama = 
    LLMRepository
    { request  = ollamaRequest
    }


ollamaRequest :: LLMRequest -> IO (Either String LLMResponse)
ollamaRequest llmReq = do
    result <- ollamaPost llmReq
    case result of
        Left  err -> pure $ Left  $ "An exception occurred while send request:" ++ show err
        Right res -> pure $ Right $ toDomain res

ollamaPost :: LLMRequest -> IO (Either HttpException OllamaResponse)
ollamaPost llmReq = try $
    runReq defaultHttpConfig $ do
        response <-
            req
                POST
                (http "localhost" /: "api" /: "chat")
                (ReqBodyJson (reqToJSON llmReq))
                jsonResponse
                (port 11434)  
         
        pure $ responseBody response

reqToJSON :: LLMRequest -> Value
reqToJSON req =
    object
    [ "model"    .= (model req)
    , "messages" .= map msgToJSON (llmMessages req)
    , "stream"   .= False
    ]

msgToJSON :: Message -> Value
msgToJSON msg =
    object
    [ "role"    .= roleToText (role msg)
    , "content" .= content msg
    ]

roleToText :: Role -> String
roleToText System    = "system"
roleToText User      = "user"
roleToText Assistant = "assistant"



data OllamaMessage = OllamaMessage
    { ollamaRole    :: Text
    , ollamaContent :: Text
    } deriving (Show)

data OllamaResponse = OllamaResponse
    { ollamaMessage :: OllamaMessage
    } deriving (Show)

instance FromJSON OllamaResponse where
    parseJSON = withObject "OllamaResponse" $ \o ->
        OllamaResponse
            <$> o .: "message"

instance FromJSON OllamaMessage where
    parseJSON = withObject "OllamaMessage" $ \o ->
        OllamaMessage
            <$> o .: "role"
            <*> o .: "content"

toDomain :: OllamaResponse -> LLMResponse
toDomain ollamaRes =
    LLMResponse
    { message = 
        Message
        { role    = textToRole (ollamaRole    mes)
        , content =             ollamaContent mes
        }
    }
    where
        mes = ollamaMessage ollamaRes

textToRole :: Text -> Role
textToRole "system"    = System
textToRole "user"      = User
textToRole "assistant" = Assistant