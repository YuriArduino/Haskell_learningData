
{-# LANGUAGE OverloadedStrings #-}

module FileTransferSpec (tests) where

import Control.Concurrent
    ( MVar
    , forkIO
    , newEmptyMVar
    , putMVar
    , takeMVar
    )

import Control.Exception
    ( IOException
    , bracket
    , displayException
    , try
    )

import qualified Data.ByteString as BS
import Data.Word (Word64)

import FileTransfer
    ( receiveFile
    , sendFile
    , transferChunkSize
    )

import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , listDirectory
    , removeFile
    , removePathForcibly
    )

import System.FilePath ((</>))

import System.IO
    ( Handle
    , hClose
    , hSetBinaryMode
    , openBinaryTempFile
    )

import System.Posix.IO (createPipe, fdToHandle)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
    ( Assertion
    , assertEqual
    , assertFailure
    , testCase
    )


-- Temporary directory helper

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory action = do
    base <- getTemporaryDirectory

    bracket
        (createTemporaryDirectory base)
        removePathForcibly
        action
  where
    createTemporaryDirectory base = do
        (path, handle) <- openBinaryTempFile base "hsTopy-ft"

        hClose handle
        removeFile path
        createDirectory path

        pure path


-- Pipe helpers

withPipe :: (Handle -> Handle -> IO a) -> IO a
withPipe action =
    bracket openPipe closePipe (uncurry action)


openPipe :: IO (Handle, Handle)
openPipe = do
    (readFd, writeFd) <- createPipe

    reader <- fdToHandle readFd
    writer <- fdToHandle writeFd

    hSetBinaryMode reader True
    hSetBinaryMode writer True

    pure (reader, writer)


safeClose :: Handle -> IO ()
safeClose handle = do
    _ <- try (hClose handle) :: IO (Either IOException ())
    pure ()


closePipe :: (Handle, Handle) -> IO ()
closePipe (reader, writer) = do
    safeClose reader
    safeClose writer


-- Sender thread

startSender
    :: Handle
    -> FilePath
    -> IO (MVar (Either IOException (Word64, Word64)))

startSender writer sourcePath = do
    done <- newEmptyMVar

    _ <- forkIO $ do
        result <- try (sendFile writer sourcePath)
            :: IO (Either IOException (Word64, Word64))

        safeClose writer
        putMVar done result

    pure done


finishSender
    :: MVar (Either IOException (Word64, Word64))
    -> Word64
    -> Word64
    -> Assertion

finishSender done expectedBytes expectedChunks = do
    result <- takeMVar done

    case result of
        Left err ->
            assertFailure (displayException err)

        Right (actualBytes, actualChunks) -> do
            assertEqual "Sent bytes" expectedBytes actualBytes
            assertEqual "Sent chunks" expectedChunks actualChunks


-- Deterministic binary content

binaryPattern :: Int -> BS.ByteString
binaryPattern size =
    BS.take size $
        BS.concat $
            replicate ((size + 255) `div` 256)
                (BS.pack [0..255])


-- Expected number of chunks

expectedChunkCount :: Int -> Word64
expectedChunkCount size
    | size == 0 = 0
    | otherwise =
        fromIntegral
            ((size + transferChunkSize - 1) `div` transferChunkSize)


-- Generic round-trip test

testFileTransferRoundTrip :: BS.ByteString -> Assertion
testFileTransferRoundTrip content =
    withTempDirectory $ \root -> do

        let sourceDir = root </> "source"
            outputDir = root </> "output"
            sourcePath = sourceDir </> "input.bin"

        createDirectory sourceDir
        createDirectory outputDir

        BS.writeFile sourcePath content

        withPipe $ \reader writer -> do

            done <- startSender writer sourcePath

            receivedPath <- receiveFile reader outputDir

            finishSender
                done
                (fromIntegral (BS.length content))
                (expectedChunkCount (BS.length content))

            actual <- BS.readFile receivedPath

            assertEqual
                "Reconstructed file"
                content
                actual

            entries <- listDirectory outputDir

            assertEqual
                "No temporary files remain"
                ["input.bin"]
                entries


-- FT01 - FT06

tests :: TestTree
tests =
    testGroup "File Transfer"
        [ testCase "FT01 - Empty file"
            (testFileTransferRoundTrip BS.empty)

        , testCase "FT02 - Small text file"
            (testFileTransferRoundTrip
                "Hello from hsTopy\n")

        , testCase "FT03 - Exact chunk size"
            (testFileTransferRoundTrip
                (BS.replicate transferChunkSize 0xA5))

        , testCase "FT04 - Chunk plus one byte"
            (testFileTransferRoundTrip
                (binaryPattern (transferChunkSize + 1)))

        , testCase "FT05 - Multiple chunks"
            (testFileTransferRoundTrip
                (binaryPattern (5 * 1024 * 1024 + 123)))

        , testCase "FT06 - Arbitrary binary content"
            (testFileTransferRoundTrip
                (binaryPattern 4096))
        ]
