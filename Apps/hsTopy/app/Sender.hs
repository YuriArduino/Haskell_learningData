{-# LANGUAGE OverloadedStrings #-}

module Sender (main) where

import Data.ProtoLens (defMessage, encodeMessage)
import Lens.Micro ((&), (.~))
import System.IO (stdout)
import qualified Proto.Message as P
import qualified Proto.Message_Fields as P

-- Importa a lógica de enquadramento do seu novo módulo
import Framing (writeFrame)

messages :: [P.Message]
messages =
    [ defMessage
        & P.content .~ "Hello from Haskell #1"
        & P.value   .~ 1

    , defMessage
        & P.content .~ "Hello from Haskell #2"
        & P.value   .~ 2

    , defMessage
        & P.content .~ "Hello from Haskell #3"
        & P.value   .~ 3
    ]

main :: IO ()
main = mapM_ (writeFrame stdout . encodeMessage) messages
