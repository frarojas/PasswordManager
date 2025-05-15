-- app/Main.hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteArray as BA
import Control.Monad (zipWithM_)
import System.Directory (doesFileExist)
import qualified Encryption as E
import System.IO (hFlush, stdout)
import System.IO (hSetEcho, stdin)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import Control.Exception (try, SomeException)
import System.Process (callCommand)
import Control.Concurrent (threadDelay)
import Text.Read (readMaybe)

-- Leer entrada oculta (para PIN o contraseñas)
getHiddenInput :: String -> IO B.ByteString
getHiddenInput prompt = do
  putStr prompt >> hFlush stdout
  hSetEcho stdin False
  input <- getLine
  hSetEcho stdin True
  putStrLn ""
  return $ B.pack input

-- Tipo de datos para almacenar entradas de contraseñas
data PasswordEntry = PasswordEntry
  { service :: String
  , username :: String
  , encryptedPassword :: B.ByteString
  } deriving (Show)

-- Creando instancias JSON para serialización/deserialización
instance ToJSON PasswordEntry where
  toJSON entry = Aeson.object
    [ "service" .= service entry
    , "username" .= username entry
    , "password" .= B.unpack (Base64.encode (encryptedPassword entry))
    ]

instance FromJSON PasswordEntry where
  parseJSON = Aeson.withObject "PasswordEntry" $ \v -> do
    srv <- v .: "service"
    usr <- v .: "username"
    pwdBase64Str <- v .: "password"
    let pwdBase64 = B.pack pwdBase64Str
    case Base64.decode pwdBase64 of
      Right pwd -> return $ PasswordEntry srv usr pwd
      Left err -> fail $ "Error decoding password: " ++ err

-- Ruta al archivo de contraseñas
passwordFile :: String
passwordFile = "passwords.json"

-- Función para guardar contraseñas en un archivo JSON
savePasswords :: [PasswordEntry] -> IO ()
savePasswords entries = do
  let json = Aeson.encode entries
  BL.writeFile passwordFile json
  putStrLn $ "Contraseñas guardadas en " ++ passwordFile

-- Función para cargar contraseñas desde un archivo JSON
loadPasswords :: IO [PasswordEntry]
loadPasswords = do
  fileExists <- doesFileExist passwordFile
  if fileExists
    then do
      result <- try $ do
        content <- BL.readFile passwordFile
        case Aeson.eitherDecode content of
          Right entries -> return entries
          Left err -> do
            putStrLn $ "Error decodificando el archivo de contraseñas: " ++ err
            return []
      case result of
        Right entries -> return entries
        Left (e :: SomeException) -> do
          putStrLn $ "Error leyendo el archivo de contraseñas: " ++ show e
          return []
    else return []

-- Función para agregar una nueva contraseña
addPassword :: BA.ScrubbedBytes -> [PasswordEntry] -> IO [PasswordEntry]
addPassword key entries = do
  putStrLn "\n=== Agregar Nueva Contraseña ==="
  putStr "Servicio o sitio web: "
  hFlush stdout
  serviceName <- getLine
  
  putStr "Nombre de usuario o correo: "
  hFlush stdout
  userName <- getLine
  
  plainPassword <- getHiddenInput "Contraseña: "
  
  -- Encriptar la contraseña
  encryptedPwd <- E.encryptText key plainPassword
  let newEntry = PasswordEntry 
        { service = serviceName
        , username = userName
        , encryptedPassword = encryptedPwd
        }
      updatedEntries = newEntry : entries
      
  -- Guardar las contraseñas actualizadas
  savePasswords updatedEntries
  putStrLn "Contraseña agregada exitosamente."
  
  return updatedEntries

-- Función para copiar contraseña al portapapeles
copyToClipboard :: String -> IO Bool
copyToClipboard text = do
  result <- try $ do
    -- Escapar comillas en el texto para PowerShell
    let escapedText = concatMap escapeChar text
        psCommand = "Set-Clipboard -Value '" ++ escapedText ++ "'"
    -- Ejecutar Set-Clipboard de PowerShell
    callCommand $ "powershell -Command \"" ++ psCommand ++ "\""
    return True
  case result of
    Right success -> return success
    Left (e :: SomeException) -> do
      putStrLn $ "Error al copiar al portapapeles: " ++ show e
      return False
  where
    escapeChar '\'' = "''"  -- Escapar comillas simples para PowerShell
    escapeChar c    = [c]

-- Función para limpiar el portapapeles
clearClipboard :: IO ()
clearClipboard = do
  -- Limpiar portapapeles copiando una cadena vacía
  _ <- (try $ callCommand "powershell -Command \"echo off | clip\"") :: IO (Either SomeException ())
  return ()

-- Función para mostrar contraseñas almacenadas
viewPasswords :: BA.ScrubbedBytes -> [PasswordEntry] -> IO ()
viewPasswords key entries = do
  putStrLn "\n=== Contraseñas Almacenadas ==="
  if null entries
    then do
      putStrLn "No hay contraseñas almacenadas."
      putStrLn "Presione Enter para continuar..."
      _ <- getLine
      return ()
    else do
      putStrLn "Número | Servicio | Usuario | Contraseña"
      putStrLn "--------------------------------------"
      -- Crear una lista de contraseñas descifradas para usar después
      decryptedPasswords <- mapM (\entry -> do
          let plainPwd = E.decryptText key (encryptedPassword entry)
          return (entry, plainPwd)
        ) entries
      
      -- Mostrar las contraseñas
      zipWithM_ (\(i :: Integer) (entry, plainPwd) -> do
          putStrLn $ show i ++ ". " ++ service entry ++ " | " ++ 
                     username entry ++ " | " ++ B.unpack plainPwd
        ) [1..] decryptedPasswords
      
      putStrLn "\n¿Desea copiar alguna contraseña al portapapeles? (S/N): "
      choice <- getLine
      case choice of
        "S" -> handleCopyPassword entries decryptedPasswords
        "s" -> handleCopyPassword entries decryptedPasswords
        _   -> return ()
  where
    handleCopyPassword :: [PasswordEntry] -> [(PasswordEntry, B.ByteString)] -> IO ()
    handleCopyPassword entriesList decryptedPasswords = do
      putStr "Ingrese el número de la contraseña a copiar: "
      hFlush stdout
      numStr <- getLine
      case (readMaybe numStr :: Maybe Integer) of
        Nothing -> do
          putStrLn "Entrada inválida. Debe ingresar un número."
          putStrLn "Presione Enter para continuar..."
          _ <- getLine
          return ()
        Just num -> do
          if num > 0 && num <= fromIntegral (length entriesList)
            then do
              let (_, plainPwd) = decryptedPasswords !! (fromIntegral num - 1)
              success <- copyToClipboard (B.unpack plainPwd)
              if success
                then do
                  putStrLn "Contraseña copiada al portapapeles. Se borrará en 30 segundos."
                  -- Iniciar un hilo para limpiar el portapapeles después de 30 segundos
                  threadDelay (30 * 1000000)  -- 30 segundos en microsegundos
                  clearClipboard
                  putStrLn "Portapapeles limpiado por seguridad."
                else 
                  putStrLn "Error al copiar la contraseña al portapapeles."
            else
              putStrLn "Número de entrada inválido."
          putStrLn "Presione Enter para continuar..."
          _ <- getLine
          return ()

mainMenu :: BA.ScrubbedBytes -> [PasswordEntry] -> IO ()
mainMenu key entries = do
  putStrLn "\n===== Password Manager ====="
  putStrLn "1. Ver contraseñas"
  putStrLn "2. Agregar nueva contraseña"
  putStrLn "3. Modificar contraseña"
  putStrLn "4. Eliminar contraseña"
  putStrLn "5. Salir"
  putStr "Seleccione una opción: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> do
      viewPasswords key entries
      mainMenu key entries
    "2" -> do
      updatedEntries <- addPassword key entries
      mainMenu key updatedEntries
    "3" -> putStrLn "Función: Modificar contraseña (pendiente)" >> mainMenu key entries
    "4" -> putStrLn "Función: Eliminar contraseña (pendiente)" >> mainMenu key entries
    "5" -> putStrLn "Saliendo..."
    _   -> putStrLn "Opción no válida." >> mainMenu key entries

main :: IO ()
main = do
  pin <- getHiddenInput "Ingrese su PIN: "
  let key = E.generateKey pin
  -- Cargar las contraseñas existentes
  entries <- loadPasswords
  putStrLn "Autenticación exitosa."
  mainMenu key entries
