module Framing
    ( headerSize
    , maxFrameSize
    , readExactly
    , receiveFrame
    , writeFrame
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder (byteString, toLazyByteString, word32BE)
import Data.Bits ((.|.), shiftL)
import Data.Word (Word32)
import System.IO (Handle, hFlush)


headerSize :: Int
headerSize = 4

maxFrameSize :: Int
maxFrameSize = 16 * 1024 * 1024

-- | Lê exatamente a quantidade de bytes solicitada de um Handle.
readExactly :: Handle -> Int -> IO (Maybe BS.ByteString)
readExactly handle requested
    | requested < 0 = ioError (userError "Invalid read size")
    | otherwise     = go requested []
  where
    go 0 chunks = pure (Just (BS.concat (reverse chunks)))
    go remaining chunks = do
        chunk <- BS.hGetSome handle remaining
        if BS.null chunk
            then if null chunks
                then pure Nothing
                else ioError . userError $
                    "Truncated stream: expected " ++ show requested ++
                    " bytes, received " ++ show (requested - remaining)
            else do
                go (remaining - BS.length chunk) (chunk : chunks)

-- | Decodifica um cabeçalho de 4 bytes big-endian.
decodeFrameSize :: BS.ByteString -> Word32
decodeFrameSize header =
        (fromIntegral (BS.index header 0) `shiftL` 24)
    .|. (fromIntegral (BS.index header 1) `shiftL` 16)
    .|. (fromIntegral (BS.index header 2) `shiftL` 8)
    .|.  fromIntegral (BS.index header 3)

-- | Recebe um único frame completo de um Handle.
receiveFrame :: Handle -> IO (Maybe BS.ByteString)
receiveFrame handle = do
    headerResult <- readExactly handle headerSize
    case headerResult of
        Nothing -> pure Nothing
        Just header -> do
            let frameSize = fromIntegral (decodeFrameSize header)
            if frameSize > maxFrameSize
                then ioError (userError "Frame exceeds maximum size")
                else do
                    payloadResult <- readExactly handle frameSize
                    case payloadResult of
                        Nothing -> ioError (userError "Unexpected EOF while reading payload")
                        Just payload -> pure (Just payload)

-- | Escreve um payload em um Handle adicionando o cabeçalho de tamanho (e fragmentando).
writeFrame :: Handle -> BS.ByteString -> IO ()
writeFrame handle payload
    | BS.length payload > maxFrameSize = ioError (userError "Frame exceeds maximum size")
    | otherwise = do
        let frame = toLazyByteString $
                word32BE (fromIntegral (BS.length payload))
                <> byteString payload
        BL.hPut handle frame
        hFlush handle
