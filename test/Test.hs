{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteArray as BA
import Encryption

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Encryption Tests"
  [ unitTests
  , propertyTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Key generation produces correct length" $
      let key = generateKey "test-pin"
      in BA.length key @?= 32  -- 256 bits = 32 bytes for AES-256

  , testCase "Empty string encryption/decryption" $ do
      let key = generateKey "test-pin"
          emptyText = ""
      encrypted <- encryptText key emptyText
      decryptText key encrypted @?= emptyText

  , testCase "Basic string encryption/decryption" $ do
      let key = generateKey "test-pin"
          plaintext = "Hello, World!"
      encrypted <- encryptText key plaintext
      decryptText key encrypted @?= plaintext

  , testCase "Special characters encryption/decryption" $ do
      let key = generateKey "test-pin"
          plaintext = "!@#$%^&*()_+-=[]{}|;:,.<>?"
      encrypted <- encryptText key plaintext
      decryptText key encrypted @?= plaintext

  , testCase "Different keys produce different ciphertexts" $ do
      let key1 = generateKey "pin1"
          key2 = generateKey "pin2"
          plaintext = "test message"
      encrypted1 <- encryptText key1 plaintext
      encrypted2 <- encryptText key2 plaintext
      encrypted1 /= encrypted2 @? "Different keys should produce different ciphertexts"

  , testCase "Unicode characters encryption/decryption" $ do
      let key = generateKey "test-pin"
          plaintext = "Hello 世界 χαίρετε"
      encrypted <- encryptText key plaintext
      decryptText key encrypted @?= plaintext
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ QC.testProperty "Encryption/decryption roundtrip preserves data" $ \pin text -> QC.ioProperty $ do
      let key = generateKey (B.pack pin)
          plaintext = B.pack text
      encrypted <- encryptText key plaintext
      return $ decryptText key encrypted == plaintext

  , QC.testProperty "Different inputs produce different ciphertexts" $ \pin text1 text2 ->
      text1 /= text2 ==> QC.ioProperty $ do
        let key = generateKey (B.pack pin)
        encrypted1 <- encryptText key (B.pack text1)
        encrypted2 <- encryptText key (B.pack text2)
        return $ encrypted1 /= encrypted2

  , QC.testProperty "Same input with different keys produces different ciphertexts" $ \pin1 pin2 text ->
      pin1 /= pin2 ==> QC.ioProperty $ do
        let key1 = generateKey (B.pack pin1)
            key2 = generateKey (B.pack pin2)
            plaintext = B.pack text
        encrypted1 <- encryptText key1 plaintext
        encrypted2 <- encryptText key2 plaintext
        return $ encrypted1 /= encrypted2

  , QC.testProperty "Key generation is consistent" $ \pin ->
      let key1 = generateKey (B.pack pin)
          key2 = generateKey (B.pack pin)
      in key1 == key2
  ]
