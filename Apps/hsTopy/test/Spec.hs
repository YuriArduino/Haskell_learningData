
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
    ( MVar
    , forkIO
    , newEmptyMVar
    , putMVar
    , takeMVar
    , threadDelay
    )

import Control.Exception
    ( IOException
    , bracket
    , displayException
    , try
    )

import Control.Monad (replicateM)
import Data.List (isInfixOf)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.ByteString.Builder
    ( byteString
    , toLazyByteString
    , word32BE
    )

import System.IO
    ( Handle
    , hClose
    , hFlush
    , hSetBinaryMode
    )

import System.Posix.IO (createPipe, fdToHandle)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit
    ( Assertion
    , assertBool
    , assertEqual
    , assertFailure
    , testCase
    )

import Framing

import Data.Char (digitToInt, isSpace)

import Data.ProtoLens
    ( defMessage
    , encodeMessage
    , decodeMessage
    )

import Lens.Micro ((&), (.~), (^.))

import qualified Proto.Message as P
import qualified Proto.Message_Fields as P

import Paths_hsTopy (getDataFileName)

main :: IO ()
main = defaultMain $
    testGroup "Framing"
        [ testGroup "F01 - Encoding"
            [ testCase "encodes big-endian header" testEncoding
            , testCase "accepts empty payload" testEmptyPayload
            ]
        , testGroup "F02 - Fragmentation"
            [ testCase "reassembles fragmented frame" testFragmentedFrame
            , testCase "receives multiple frames" testMultipleFrames
            ]
        , testGroup "F03 - EOF and truncation"
            [ testCase "accepts clean EOF" testCleanEOF
            , testCase "detects truncated header" testTruncatedHeader
            , testCase "detects truncated payload" testTruncatedPayload
            , testCase "detects truncation after valid frame" testF10ValidThenTruncated
            ]
        , testGroup "F04 - Validation"
            [ testCase "rejects oversized incoming frame" testOversizedIncoming
            , testCase "rejects oversized outgoing frame" testOversizedOutgoing
            ]
        , testGroup "I - Interoperability"
            [ testCase "I01 - Haskell frame encoding" testI01FrameEncoding
            , testCase "I02 - Haskell frame decoding" testI02FrameDecoding
            , testCase "I03 - Haskell protobuf encoding" testI03ProtobufEncoding
            , testCase "I04 - Haskell protobuf decoding" testI04ProtobufDecoding
            , testCase "I05 - Multiple frames" testI05MultipleFrames
            , testCase "F09 - Framing isolates invalid protobuf payload" testF09InvalidProtobuf
            ]
        ]



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


closePipe :: (Handle, Handle) -> IO ()
closePipe (reader, writer) = do
    safeClose reader
    safeClose writer


safeClose :: Handle -> IO ()
safeClose handle = do
    _ <- try (hClose handle) :: IO (Either IOException ())
    pure ()

-- read vector helpers

readHexVector :: FilePath -> IO BS.ByteString
readHexVector name = do
    path <- getDataFileName ("test-vectors/" ++ name)
    contents <- readFile path

    pure $ BS.pack $ decodeHex (filter (not . isSpace) contents)
  where
    decodeHex [] = []
    decodeHex (hi:lo:rest) =
        fromIntegral (digitToInt hi * 16 + digitToInt lo)
        : decodeHex rest
    decodeHex _ =
        error "Invalid hexadecimal test vector"


decodePayload :: BS.ByteString -> IO P.Message
decodePayload payload =
    case decodeMessage payload of
        Left err ->
            assertFailure ("Protobuf decoding failed: " ++ err)

        Right message ->
            pure message


hello42 :: P.Message
hello42 =
    defMessage
        & P.content .~ "Hello"
        & P.value   .~ 42


messageA :: P.Message
messageA =
    defMessage
        & P.content .~ "A"
        & P.value   .~ 1


messageBC :: P.Message
messageBC =
    defMessage
        & P.content .~ "BC"
        & P.value   .~ 2


-- Writer thread helpers

startWriter :: Handle -> IO () -> IO (MVar (Either IOException ()))
startWriter handle action = do
    done <- newEmptyMVar

    _ <- forkIO $ do
        result <- try action :: IO (Either IOException ())
        safeClose handle
        putMVar done result

    pure done


finishWriter :: MVar (Either IOException ()) -> Assertion
finishWriter done = do
    result <- takeMVar done

    case result of
        Left err -> assertFailure (displayException err)
        Right () -> pure ()


-- Test-only framing encoder

encodeTestFrame :: BS.ByteString -> BS.ByteString
encodeTestFrame payload =
    BL.toStrict $
        toLazyByteString $
            word32BE (fromIntegral (BS.length payload))
            <> byteString payload


-- Deliberately fragmented writer used only by tests

writeFragments :: Handle -> BS.ByteString -> IO ()
writeFragments handle = go (cycle [1, 2, 1, 3, 1, 2, 4])
  where
    go _ remaining | BS.null remaining = pure ()

    go (chunkSize : sizes) remaining = do
        let (chunk, rest) = BS.splitAt chunkSize remaining

        BS.hPut handle chunk
        hFlush handle

        if BS.null rest
            then pure ()
            else threadDelay 5000

        go sizes rest

    go [] remaining =
        go (cycle [1, 2, 1, 3, 1, 2, 4]) remaining


-- Assertion helpers

assertIOExceptionContaining :: String -> IO a -> Assertion
assertIOExceptionContaining expected action = do
    result <- try (action >> pure ()) :: IO (Either IOException ())

    case result of
        Left err ->
            assertBool
                ("Unexpected error: " ++ displayException err)
                (expected `isInfixOf` displayException err)

        Right () ->
            assertFailure ("Expected an error containing: " ++ expected)


-- F01: Encoding

testEncoding :: Assertion
testEncoding = withPipe $ \reader writer -> do
    let payload = "abc"
        expected = BS.pack [0, 0, 0, 3] <> payload

    writeFrame writer payload
    hClose writer

    actual <- BS.hGetContents reader

    assertEqual "Header and payload" expected actual


testEmptyPayload :: Assertion
testEmptyPayload = withPipe $ \reader writer -> do
    writeFrame writer BS.empty
    hClose writer

    actual <- receiveFrame reader

    assertEqual "Empty payload" (Just BS.empty) actual


-- F02: Fragmentation

testFragmentedFrame :: Assertion
testFragmentedFrame = withPipe $ \reader writer -> do
    let payload = "fragmented payload"
        wire = encodeTestFrame payload

    done <- startWriter writer (writeFragments writer wire)

    actual <- receiveFrame reader

    finishWriter done

    assertEqual "Reconstructed payload" (Just payload) actual


testMultipleFrames :: Assertion
testMultipleFrames = withPipe $ \reader writer -> do
    let payloads = ["first", "second", "third"]
        wire = BS.concat (map encodeTestFrame payloads)

    done <- startWriter writer (writeFragments writer wire)

    actual <- replicateM (length payloads) (receiveFrame reader)
    eof <- receiveFrame reader

    finishWriter done

    assertEqual
        "Multiple frames"
        (map Just payloads)
        actual

    assertEqual "EOF after final frame" Nothing eof


-- F03: EOF and truncation

testCleanEOF :: Assertion
testCleanEOF = withPipe $ \reader writer -> do
    hClose writer

    actual <- receiveFrame reader

    assertEqual "Clean EOF" Nothing actual


testTruncatedHeader :: Assertion
testTruncatedHeader = withPipe $ \reader writer -> do
    BS.hPut writer (BS.pack [0, 0])
    hClose writer

    assertIOExceptionContaining
        "Truncated stream"
        (receiveFrame reader)


testTruncatedPayload :: Assertion
testTruncatedPayload = withPipe $ \reader writer -> do
    -- Header declares 3 bytes, but only 2 are transmitted.
    BS.hPut writer (BS.pack [0, 0, 0, 3, 97, 98])
    hClose writer

    assertIOExceptionContaining
        "Truncated stream"
        (receiveFrame reader)


-- F04: Validation

testOversizedIncoming :: Assertion
testOversizedIncoming = withPipe $ \reader writer -> do
    -- 16 MiB + 1
    BS.hPut writer (BS.pack [1, 0, 0, 1])
    hClose writer

    assertIOExceptionContaining
        "Frame exceeds maximum size"
        (receiveFrame reader)


testOversizedOutgoing :: Assertion
testOversizedOutgoing = withPipe $ \_ writer ->
    assertIOExceptionContaining
        "Frame exceeds maximum size"
        (writeFrame writer (BS.replicate (maxFrameSize + 1) 0))


testF09InvalidProtobuf :: Assertion
testF09InvalidProtobuf = withPipe $ \reader writer -> do
    let malformed = BS.pack [0x0a, 0x05, 0x41]

    -- Framing must preserve the payload.
    writeFrame writer malformed
    hClose writer

    payload <- receiveFrame reader

    assertEqual
        "Framing preserves opaque payload"
        (Just malformed)
        payload

    -- Protobuf decoding must reject the malformed payload.
    case (decodeMessage malformed :: Either String P.Message) of
        Left _ ->
            pure ()

        Right _ ->
            assertFailure "Expected malformed protobuf to be rejected"

testF10ValidThenTruncated :: Assertion
testF10ValidThenTruncated = withPipe $ \reader writer -> do
    let validFrame = encodeTestFrame "first"
        incompleteHeader = BS.pack [0, 0]

    BS.hPut writer (validFrame <> incompleteHeader)
    hClose writer

    first <- receiveFrame reader

    assertEqual
        "First frame remains valid"
        (Just "first")
        first

    assertIOExceptionContaining
        "Truncated stream"
        (receiveFrame reader)


-- I01: Haskell produces the shared framing vector.

testI01FrameEncoding :: Assertion
testI01FrameEncoding = do
    expected <- readHexVector "frame_abc.hex"

    withPipe $ \reader writer -> do
        writeFrame writer "abc"
        hClose writer

        actual <- BS.hGetContents reader

        assertEqual "Frame bytes" expected actual


-- I02: Haskell consumes the shared framing vector.

testI02FrameDecoding :: Assertion
testI02FrameDecoding = do
    wire <- readHexVector "frame_abc.hex"

    withPipe $ \reader writer -> do
        BS.hPut writer wire
        hClose writer

        actual <- receiveFrame reader

        assertEqual "Decoded payload" (Just "abc") actual


-- I03: Haskell produces the shared protobuf vector.

testI03ProtobufEncoding :: Assertion
testI03ProtobufEncoding = do
    expected <- readHexVector "protobuf_hello_42.hex"

    let actual = encodeMessage hello42

    assertEqual "Protobuf bytes" expected actual


-- I04: Haskell consumes the shared protobuf vector.

testI04ProtobufDecoding :: Assertion
testI04ProtobufDecoding = do
    payload <- readHexVector "protobuf_hello_42.hex"

    message <- decodePayload payload

    assertEqual "Content" "Hello" (message ^. P.content)
    assertEqual "Value" 42 (message ^. P.value)


-- I05: Haskell produces and consumes multiple shared frames.

testI05MultipleFrames :: Assertion
testI05MultipleFrames = do
    expected <- readHexVector "multi_message_stream.hex"

    withPipe $ \reader writer -> do
        mapM_
            (writeFrame writer . encodeMessage)
            [messageA, messageBC]

        hClose writer

        actual <- BS.hGetContents reader

        assertEqual "Complete stream" expected actual

    withPipe $ \reader writer -> do
        BS.hPut writer expected
        hClose writer

        first <- receiveFrame reader
        second <- receiveFrame reader
        eof <- receiveFrame reader

        assertEqual "First frame"
            (Just (encodeMessage messageA))
            first

        assertEqual "Second frame"
            (Just (encodeMessage messageBC))
            second

        assertEqual "End of stream" Nothing eof
