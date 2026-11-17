module FileTransfer
    ( transferChunkSize
    , sendFile
    , receiveFile
    ) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (when)

import Crypto.Hash
    ( Context
    , SHA256
    , hashFinalize
    , hashInit
    , hashUpdate
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Data.Word (Word32, Word64)

import Data.ProtoLens
    ( decodeMessage
    , defMessage
    , encodeMessage
    )

import Lens.Micro ((&), (.~), (^.))

import System.Directory
    ( createDirectoryIfMissing
    , removeFile
    )

import System.FilePath (takeFileName, (</>))

import System.IO
    ( Handle
    , IOMode(ReadMode)
    , hClose
    , hFileSize
    , hFlush
    , hSetBinaryMode
    , openBinaryTempFile
    , withBinaryFile
    )

import System.Posix.Files (createLink)

import Framing (maxFrameSize, receiveFrame, writeFrame)

import qualified Proto.Message as P
import qualified Proto.Message_Fields as P



transferChunkSize :: Int
transferChunkSize = 256 * 1024


-- Common helpers

failTransfer :: String -> IO a
failTransfer = ioError . userError


writeTransfer :: Handle -> P.TransferMessage -> IO ()
writeTransfer output =
    writeFrame output . encodeMessage


decodeTransfer :: BS.ByteString -> IO P.TransferMessage
decodeTransfer payload =
    case decodeMessage payload of
        Left err ->
            failTransfer ("Invalid protobuf message: " ++ err)

        Right message ->
            pure message


-- Incremental file reading

readChunk :: Handle -> Int -> IO BS.ByteString
readChunk handle requested = go requested []
  where
    go 0 chunks =
        pure (BS.concat (reverse chunks))

    go remaining chunks = do
        part <- BS.hGetSome handle remaining

        if BS.null part
            then pure (BS.concat (reverse chunks))
            else
                go (remaining - BS.length part) (part : chunks)


-- Message construction

makeStart :: FilePath -> Word64 -> P.TransferMessage
makeStart fileName fileSize =
    let start =
            defMessage
                & P.fileName .~ T.pack fileName
                & P.fileSize .~ fileSize
                & P.chunkSize .~ fromIntegral transferChunkSize
    in
        defMessage
            & P.maybe'body .~
                Just (P.TransferMessage'Start start)


makeChunk :: Word64 -> BS.ByteString -> P.TransferMessage
makeChunk index payload =
    let chunk =
            defMessage
                & P.index .~ index
                & P.data' .~ payload
    in
        defMessage
            & P.maybe'body .~
                Just (P.TransferMessage'Chunk chunk)


makeEnd :: BS.ByteString -> P.TransferMessage
makeEnd digest =
    let ending =
            defMessage
                & P.sha256 .~ digest
    in
        defMessage
            & P.maybe'body .~
                Just (P.TransferMessage'End ending)


-- Sender

sendFile :: Handle -> FilePath -> IO (Word64, Word64)
sendFile output inputPath = do
    hSetBinaryMode output True

    withBinaryFile inputPath ReadMode $ \input -> do
        sizeInteger <- hFileSize input

        when
            (sizeInteger < 0 ||
             sizeInteger > toInteger (maxBound :: Word64))
            (failTransfer "File size exceeds supported range")

        let fileName = takeFileName inputPath
            fileSize = fromInteger sizeInteger

        when (null fileName || fileName == "." || fileName == "..") $
            failTransfer "Invalid file name"

        -- 1. Announce transfer.
        writeTransfer output (makeStart fileName fileSize)

        -- 2. Send chunks and calculate SHA-256 incrementally.
        (count, total, digest) <-
            sendChunks output input fileSize 0 0 hashInit

        when (total /= fileSize) $
            failTransfer "File size changed during transmission"

        -- 3. Send FileEnd.
        let digestBytes =
                BA.convert (hashFinalize digest)
                    :: BS.ByteString

        writeTransfer output (makeEnd digestBytes)

        hFlush output

        pure (total, count)


sendChunks
    :: Handle
    -> Handle
    -> Word64
    -> Word64
    -> Word64
    -> Context SHA256
    -> IO (Word64, Word64, Context SHA256)

sendChunks output input expectedSize index total digest = do
    payload <- readChunk input transferChunkSize

    if BS.null payload
        then pure (index, total, digest)
        else do
            let amount = fromIntegral (BS.length payload)

            when
                (total > expectedSize ||
                 amount > expectedSize - total)
                (failTransfer "File grew during transmission")

            when (index == maxBound) $
                failTransfer "Chunk index overflow"

            writeTransfer output (makeChunk index payload)

            sendChunks
                output
                input
                expectedSize
                (index + 1)
                (total + amount)
                (hashUpdate digest payload)


-- Receiver

receiveFile :: Handle -> FilePath -> IO FilePath
receiveFile input outputDir = do
    hSetBinaryMode input True

    firstFrame <- receiveFrame input

    case firstFrame of
        Nothing ->
            failTransfer "Missing FileStart"

        Just payload -> do
            message <- decodeTransfer payload

            case message ^. P.maybe'body of
                Just (P.TransferMessage'Start start) ->
                    startTransfer input outputDir start

                _ ->
                    failTransfer "Expected FileStart"


validateFileName :: String -> IO ()
validateFileName fileName =
    when
        ( null fileName
          || fileName == "."
          || fileName == ".."
          || any (`elem` ['/', '\\', '\0']) fileName
        )
        (failTransfer "Invalid file name")


startTransfer :: Handle -> FilePath -> P.FileStart -> IO FilePath
startTransfer input outputDir start = do
    let fileName = T.unpack (start ^. P.fileName)
        fileSize = start ^. P.fileSize
        chunkSize = start ^. P.chunkSize

    validateFileName fileName

    when (chunkSize == 0) $
        failTransfer "Invalid chunk size"

    when (toInteger chunkSize > toInteger maxFrameSize) $
        failTransfer "Chunk size exceeds maximum frame size"

    createDirectoryIfMissing True outputDir

    let targetPath = outputDir </> fileName

    -- Temporary file remains in the destination filesystem.
    bracket
        (openBinaryTempFile outputDir (fileName ++ ".part"))
        cleanupTempFile
        (\(tempPath, output) -> do
            hSetBinaryMode output True

            receiveChunks
                input
                output
                fileSize
                chunkSize
                0
                0
                hashInit

            hFlush output
            hClose output

            -- Atomic publication without overwriting an existing target.
            createLink tempPath targetPath
            removeFile tempPath

            pure targetPath
        )


cleanupTempFile :: (FilePath, Handle) -> IO ()
cleanupTempFile (tempPath, handle) = do
    _ <- try (hClose handle) :: IO (Either IOException ())
    _ <- try (removeFile tempPath) :: IO (Either IOException ())
    pure ()


receiveChunks
    :: Handle
    -> Handle
    -> Word64
    -> Word32
    -> Word64
    -> Word64
    -> Context SHA256
    -> IO ()

receiveChunks input output expectedSize chunkSize expectedIndex totalBytes digest = do
    nextFrame <- receiveFrame input

    case nextFrame of
        Nothing ->
            failTransfer "Unexpected EOF before FileEnd"

        Just payload -> do
            message <- decodeTransfer payload

            case message ^. P.maybe'body of

                Just (P.TransferMessage'Chunk chunk) -> do
                    let chunkIndex = chunk ^. P.index
                        chunkData = chunk ^. P.data'

                    when (chunkIndex /= expectedIndex) $
                        failTransfer "Unexpected chunk index"

                    when (totalBytes >= expectedSize) $
                        failTransfer "Unexpected extra chunk"

                    let remaining = expectedSize - totalBytes

                        expectedChunkSize =
                            min (fromIntegral chunkSize) remaining

                        actualChunkSize =
                            fromIntegral (BS.length chunkData)

                    when (actualChunkSize /= expectedChunkSize) $
                        failTransfer "Invalid chunk payload size"

                    BS.hPut output chunkData

                    receiveChunks
                        input
                        output
                        expectedSize
                        chunkSize
                        (expectedIndex + 1)
                        (totalBytes + actualChunkSize)
                        (hashUpdate digest chunkData)

                Just (P.TransferMessage'End ending) -> do
                    when (totalBytes /= expectedSize) $
                        failTransfer "File size mismatch"

                    let expectedHash = ending ^. P.sha256

                        actualHash =
                            BA.convert (hashFinalize digest)
                                :: BS.ByteString

                    when (BS.length expectedHash /= 32) $
                        failTransfer "Invalid SHA-256 length"

                    when (expectedHash /= actualHash) $
                        failTransfer "SHA-256 mismatch"

                    trailingFrame <- receiveFrame input

                    case trailingFrame of
                        Nothing -> pure ()
                        Just _ ->
                            failTransfer "Unexpected data after FileEnd"

                Just (P.TransferMessage'Start _) ->
                    failTransfer "Unexpected FileStart"

                Nothing ->
                    failTransfer "TransferMessage has no body"
