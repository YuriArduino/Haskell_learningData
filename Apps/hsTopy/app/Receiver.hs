module Receiver (main) where

import Data.ProtoLens (decodeMessage)
import Lens.Micro ((^.))
import System.IO (stdin)

-- Importa o enquadramento modular
import Framing (receiveFrame)

import qualified Proto.Message as P
import qualified Proto.Message_Fields as P

receiveFrames :: Int -> IO ()
receiveFrames index = do
    frameResult <- receiveFrame stdin
    case frameResult of
        Nothing -> pure ()
        Just payload ->
            case (decodeMessage payload :: Either String P.Message) of
                Left err -> ioError (userError ("Invalid protobuf payload: " ++ err))
                Right message -> do
                    putStrLn $ "[" ++ show index ++ "] content: " ++ show (message ^. P.content)
                    putStrLn $ "[" ++ show index ++ "] value: "   ++ show (message ^. P.value)
                    receiveFrames (index + 1)

main :: IO ()
main = receiveFrames 1
