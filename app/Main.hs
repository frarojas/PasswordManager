-- app/Main.hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteArray as BA
import Control.Monad (zipWithM_, void, forM) -- Added void and forM
import System.Directory (doesFileExist, createDirectoryIfMissing)
import qualified Encryption as E
import System.IO (hFlush, stdout, hSetEcho, stdin, withFile, IOMode(..)) -- Removed Handle
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..)) -- Removed object
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import Control.Exception (try, SomeException, IOException, catch)
import System.Process (callCommand)
import Control.Concurrent (threadDelay)
import Text.Read (readMaybe)
import Control.Monad.Trans.RWS (put)

-- Leer entrada oculta (para PIN o contraseñas)
getHiddenInput :: String -> IO B.ByteString
getHiddenInput prompt = do
  putStr prompt >> hFlush stdout
  _ <- hSetEcho stdin False
  input <- getLine
  _ <- hSetEcho stdin True
  putStrLn ""
  return $ B.pack input

-- Tipo de datos para almacenar entradas de contraseñas
data PasswordEntry = PasswordEntry
  { service :: String
  , username :: String
  , encryptedPassword :: B.ByteString
  } deriving (Show, Eq)

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

-- Ruta al directorio de datos de usuario
userDataDir :: String
userDataDir = "data/"

-- Construir la ruta al archivo de contraseñas del usuario
userPasswordFile :: String -> String
userPasswordFile appUsername = userDataDir ++ appUsername ++ ".json"

-- Función para guardar contraseñas y PIN de un usuario
saveUserPasswords :: String -> B.ByteString -> [PasswordEntry] -> IO ()
saveUserPasswords appUsername pin entries = do
  createDirectoryIfMissing True userDataDir
  let filePath = userPasswordFile appUsername
      json = Aeson.encode entries
  withFile filePath WriteMode $ \h -> do
    B.hPutStrLn h pin 
    BL.hPutStr h json 

-- Función para cargar PIN y contraseñas de un usuario
loadUserPasswords :: String -> IO (Maybe (B.ByteString, [PasswordEntry]))
loadUserPasswords appUsername = do
  let filePath = userPasswordFile appUsername
  fileExists <- doesFileExist filePath
  if fileExists
    then catch (do
      withFile filePath ReadMode $ \h -> do
        pinFromFile <- B.hGetLine h
        -- Limpiar el PIN de caracteres de nueva línea y retorno de carro
        let cleanedPin = B.filter (\c -> c /= '\r' && c /= '\n') pinFromFile
        jsonContent <- BL.hGetContents h
        case Aeson.decode jsonContent of
          Just entries -> do
            return $ Just (cleanedPin, entries)
          Nothing -> do
            putStrLn $ "Error: Formato JSON inválido en " ++ filePath
            return Nothing)
      (\(e :: IOException) -> do
        putStrLn $ "Error al leer el archivo " ++ filePath ++ ": " ++ show e
        return Nothing)
    else return Nothing

-- Función para crear un nuevo usuario
createNewUser :: String -> IO (Maybe (B.ByteString, [PasswordEntry]))
createNewUser appUsername = do
  putStrLn $ "El usuario '" ++ appUsername ++ "' no existe."
  putStr "Desea crear un nuevo usuario? (s/n): "
  hFlush stdout
  confirm <- getLine
  if confirm == "s" || confirm == "S"
    then do
      newPin <- getHiddenInput "Ingrese un nuevo PIN para este usuario (numérico recomendado): "
      saveUserPasswords appUsername newPin [] 
      putStrLn $ "Usuario '" ++ appUsername ++ "' creado exitosamente."
      return $ Just (newPin, [])
    else do
      putStrLn "Creación de usuario cancelada."
      return Nothing

-- Función para agregar una nueva contraseña
addPassword :: BA.ScrubbedBytes -> String -> B.ByteString -> [PasswordEntry] -> IO [PasswordEntry]
addPassword key appUsername userPin entries = do
  putStrLn "\n=== Agregar Nueva Contraseña ==="
  putStr "Servicio o sitio web: "
  hFlush stdout
  serviceName <- getLine
  
  putStr "Nombre de usuario o correo: "
  hFlush stdout
  serviceUserName <- getLine 
  
  plainPassword <- getHiddenInput "Contraseña: "
  
  -- Encriptar la contraseña
  encryptedPwd <- E.encryptText key plainPassword
  let newEntry = PasswordEntry 
        { service = serviceName
        , username = serviceUserName 
        , encryptedPassword = encryptedPwd
        }
      updatedEntries = newEntry : entries
      
  saveUserPasswords appUsername userPin updatedEntries
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
modifyPassword :: BA.ScrubbedBytes -> String -> B.ByteString -> [PasswordEntry] -> IO [PasswordEntry]
modifyPassword key appUsername userPin entries = do
  putStrLn $ "\n=== Modificar Entrada de Contraseña para " ++ appUsername ++ " ==="
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
              saveUserPasswords appUsername userPin updatedEntries
              putStrLn "Entrada modificada exitosamente."
              return updatedEntries
            else do
              putStrLn "Número de entrada inválido."
              return entries

mainMenu :: BA.ScrubbedBytes -> String -> B.ByteString -> [PasswordEntry] -> IO ()
mainMenu key appUsername userPin entries = do
  putStrLn "\n===== Password Manager ====="
  putStrLn $ "Usuario: " ++ appUsername
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
      mainMenu key appUsername userPin entries
    "2" -> do
      updatedEntries <- addPassword key appUsername userPin entries
      mainMenu key appUsername userPin updatedEntries
    "3" -> do
      updatedEntries <- modifyPassword key appUsername userPin entries
      mainMenu key appUsername userPin updatedEntries
    "4" -> do
      putStrLn "Función de eliminación no implementada."
      mainMenu key appUsername userPin entries
    "5" -> putStrLn "Saliendo..."
    _ -> do
      putStrLn "Opción no válida, intente de nuevo."
      mainMenu key appUsername userPin entries

main :: IO ()
main = do
  putStr "Ingrese su nombre de usuario: "
  hFlush stdout
  appUsername_main <- getLine 

  loadedData <- loadUserPasswords appUsername_main
  
  case loadedData of
    Just (storedPin, entries) -> do
      inputPin <- getHiddenInput "Ingrese su PIN: "
      if inputPin == storedPin
        then do
          let key = E.generateKey inputPin
          putStrLn "Autenticación exitosa."
          mainMenu key appUsername_main inputPin entries
        else do
          putStrLn "PIN incorrecto. Saliendo."
    Nothing -> do
      maybeNewUserData <- createNewUser appUsername_main
      case maybeNewUserData of
        Just (newPin, newEntries) -> do
          let key = E.generateKey newPin
          mainMenu key appUsername_main newPin newEntries
        Nothing -> putStrLn "Saliendo."
