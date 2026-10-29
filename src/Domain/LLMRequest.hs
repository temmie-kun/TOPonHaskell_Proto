{-# LANGUAGE OverloadedStrings #-}
module Domain.LLMRequest where

import Data.Text(Text,unpack,length)

data Role
    = System
    | User
    | Assistant
    deriving (Show)

data Message = Message
    { role    :: Role
    , content :: Text
    } deriving (Show)

createMessage :: Text -> Message
createMessage content = Message { role = User, content = content }

data LLMRequest = LLMRequest
    { model       :: Text
    , llmMessages :: [Message]
    } deriving (Show)

createRequest :: [Message] -> Text -> LLMRequest
createRequest messages model = LLMRequest { model = model, llmMessages = messages }

createRequestFromText :: Text -> Text -> LLMRequest
createRequestFromText content model = createRequest [createMessage content] model

data LLMResponse = LLMResponse
    { message :: Message
    } deriving (Show)
