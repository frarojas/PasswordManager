{-# LANGUAGE OverloadedStrings #-}

module Encryption (encryptText, decryptText, generateKey) where

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..), makeIV)
import           Crypto.Error (CryptoFailable(..))
import           Crypto.KDF.PBKDF2 (Parameters(..), fastPBKDF2_SHA256)
import           Crypto.Random (getRandomBytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import           Data.Maybe (fromJust)

-- Parámetros para derivación de clave
pbkdf2Params :: Parameters
pbkdf2Params = Parameters
  { iterCounts = 10000
  , outputLength = 32 -- 256 bits para AES-256
  }

-- Deriva una clave desde el PIN usando PBKDF2
generateKey :: B.ByteString -> BA.ScrubbedBytes
generateKey pin = fastPBKDF2_SHA256 pbkdf2Params pin (B.pack "salt")

-- Encriptar un texto con una clave
encryptText :: BA.ScrubbedBytes -> B.ByteString -> IO B.ByteString
encryptText key plaintext = do
  -- Generate a random 16-byte IV
  iv <- getRandomBytes 16 :: IO B.ByteString
  let cipher = cipherInit key :: CryptoFailable AES256
      actualIV = fromJust $ makeIV iv
  case cipher of
    CryptoPassed c -> 
      -- Prepend IV to ciphertext
      return $ iv `BS.append` ctrCombine c actualIV plaintext
    CryptoFailed e -> error $ "Encryption error: " ++ show e

-- Desencriptar texto (el texto cifrado incluye el IV al principio)
decryptText :: BA.ScrubbedBytes -> B.ByteString -> B.ByteString
decryptText key ciphertext =
  let (iv, actualCiphertext) = BS.splitAt 16 ciphertext  -- Extraer IV del inicio
      cipher = cipherInit key :: CryptoFailable AES256
      actualIV = fromJust $ makeIV iv
  in case cipher of
       CryptoPassed c -> ctrCombine c actualIV actualCiphertext
       CryptoFailed e -> error $ "Decryption error: " ++ show e

