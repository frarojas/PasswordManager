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
import Control.Monad.Trans.State (modify)

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

-- función encargada de modificar un campo de una entrada
modifyPassword :: BA.ScrubbedBytes -> [PasswordEntry] -> IO [PasswordEntry]
modifyPassword key entries = do
  putStrLn "\n=== Modificar Entrada de Contraseña ==="
  if null entries
    then do
      putStrLn "No hay contraseñas almacenadas."
      putStrLn "Presione Enter para continuar..."
      _ <- getLine
      return entries
    else do
      -- Mostrar lista de entradas
      putStrLn "Número | Servicio | Usuario"
      putStrLn "-----------------------------"
      zipWithM_
        ( \(i :: Integer) entry -> do
            putStrLn $ show i ++ ". " ++ service entry ++ " | " ++ username entry
        )
        [1 ..]
        entries

      -- Selección de entrada
      putStr "Ingrese el número de la entrada a modificar: "
      hFlush stdout
      numStr <- getLine
      case (readMaybe numStr :: Maybe Integer) of
        Nothing -> do
          putStrLn "Entrada inválida. Debe ingresar un número."
          _ <- getLine
          return entries
        Just num -> do
          if num > 0 && num <= fromIntegral (length entries)
            then do
              let index = fromIntegral num - 1
              let entry = entries !! index

              -- Menú de campo a modificar
              putStrLn "\n¿Qué desea modificar?"
              putStrLn "1. Nombre del servicio"
              putStrLn "2. Nombre del usuario"
              putStrLn "3. Contraseña"
              putStr "Seleccione una opción (1-3): "
              hFlush stdout
              opt <- getLine

              updatedEntry <- case opt of
                "1" -> do
                  putStr "Nuevo nombre del servicio: "
                  hFlush stdout
                  newService <- getLine
                  return entry {service = newService}
                "2" -> do
                  putStr "Nuevo nombre de usuario: "
                  hFlush stdout
                  newUser <- getLine
                  return entry {username = newUser}
                "3" -> do
                  newPassword <- getHiddenInput $ "Nueva contraseña para " ++ service entry ++ ": "
                  encryptedPwd <- E.encryptText key newPassword
                  return entry {encryptedPassword = encryptedPwd}
                _ -> do
                  putStrLn "Opción no válida. No se realizaron cambios."
                  return entry

              -- Actualizar lista
              let updatedEntries = take index entries ++ [updatedEntry] ++ drop (index + 1) entries
              savePasswords updatedEntries
              putStrLn "Entrada modificada exitosamente."
              return updatedEntries
            else do
              putStrLn "Número de entrada inválido."
              return entries


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
    "3" -> do
      updatedEntries <- modifyPassword key entries
      mainMenu key updatedEntries 
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
