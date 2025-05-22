-- app/Main.hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteArray as BA
import Control.Monad (zipWithM_, void, forM) -- Added void and forM
import System.Directory (doesFileExist, createDirectoryIfMissing)
import qualified Encryption as E
import System.IO (hFlush, stdout, hSetEcho, stdin, withFile, IOMode(..))
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import Control.Exception (try, SomeException, IOException, catch)
import System.Process (callCommand)
import Text.Read (readMaybe)
import Data.Char (isDigit)

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
saveUserPasswords appUsername plainPin entries = do
  createDirectoryIfMissing True userDataDir
  let filePath = userPasswordFile appUsername
      -- Hashear el PIN antes de guardarlo
      pinKeyBytes = E.generateKey plainPin -- BA.ScrubbedBytes
      pinHashByteString = BA.convert pinKeyBytes :: B.ByteString
      pinHashBase64 = Base64.encode pinHashByteString
      json = Aeson.encode entries
  withFile filePath WriteMode $ \h -> do
    B.hPutStrLn h pinHashBase64 -- Guardar el hash del PIN en Base64
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

-- Función para validar el PIN
isValidPin :: String -> Bool
isValidPin pin = length pin >= 4 && all isDigit pin

-- Función para crear un nuevo usuario
createNewUser :: String -> IO (Maybe (B.ByteString, [PasswordEntry]))
createNewUser appUsername = do
  putStrLn $ "El usuario '" ++ appUsername ++ "' no existe."
  putStr "Desea crear un nuevo usuario? (s/n): "
  hFlush stdout
  confirm <- getLine
  if confirm == "s" || confirm == "S"
    then do
      newPinBS <- loopForPin
      saveUserPasswords appUsername newPinBS [] 
      putStrLn $ "Usuario '" ++ appUsername ++ "' creado exitosamente."
      return $ Just (newPinBS, [])
    else do
      putStrLn "Creación de usuario cancelada."
      return Nothing
  where
    loopForPin :: IO B.ByteString
    loopForPin = do
      pinBS <- getHiddenInput "Ingrese un nuevo PIN para este usuario (numérico, al menos 4 dígitos): "
      let pinStr = B.unpack pinBS
      if isValidPin pinStr
        then return pinBS
        else do
          putStrLn "PIN inválido. Debe ser numérico y tener al menos 4 dígitos."
          loopForPin

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

-- Función para enmascarar el nombre de usuario
maskUsername :: String -> String
maskUsername u = take 4 u ++ replicate (max 0 (8 - length (take 4 u))) '*'

padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

-- Función para mostrar contraseñas almacenadas
viewPasswords :: BA.ScrubbedBytes -> [PasswordEntry] -> IO ()
viewPasswords _ [] = do
  putStrLn "\nNo hay contraseñas almacenadas."
  putStrLn "Presione Enter para continuar..."
  _ <- getLine
  return ()
viewPasswords key entries = do
  putStrLn "\n=== Contraseñas Almacenadas ==="
  putStrLn "-------------------------------------------------------------"
  putStrLn "| índice | Servicio        | Usuario  | Contraseña |"
  putStrLn "-------------------------------------------------------------"
  mapM_ (\(idx, entry) ->
    putStrLn $ "| " ++ padRight 6 (show idx) ++
               " | " ++ padRight 15 (service entry) ++
               " | " ++ padRight 8 (maskUsername (username entry)) ++
               " | " ++ padRight 10 "**********" ++ " |"
    ) $ zip [1 :: Integer ..] entries
  putStrLn "-------------------------------------------------------------"

  putStr "\nSeleccione el índice de la entrada para ver detalles (o presione Enter para volver): "
  hFlush stdout
  choiceStr <- getLine
  case readMaybe choiceStr :: Maybe Integer of
    Just choiceIdx | choiceIdx > 0 && choiceIdx <= fromIntegral (length entries) -> do
      let selectedEntry = entries !! (fromIntegral choiceIdx - 1)
      putStrLn "\n¡ADVERTENCIA! Va a mostrar información sensible."
      putStr "Desea continuar? (s/n): "
      hFlush stdout
      confirm <- getLine
      if confirm == "s" || confirm == "S"
        then do
          let decryptedPassword = E.decryptText key (encryptedPassword selectedEntry)
          putStrLn "\n=== Detalles de la Contraseña ==="
          putStrLn $ "Servicio: " ++ service selectedEntry
          putStrLn $ "Usuario: " ++ username selectedEntry
          putStrLn $ "Contraseña: " ++ B.unpack decryptedPassword
          putStrLn "================================="
          putStrLn "\n¿Qué desea copiar al portapapeles?"
          putStrLn "1. Nombre de usuario"
          putStrLn "2. Contraseña"
          putStrLn "n. No copiar nada (o presione Enter)"
          putStr "Seleccione una opción: "
          hFlush stdout
          copyChoice <- getLine
          case copyChoice of
            "1" -> do
              copied <- copyToClipboard (username selectedEntry)
              if copied
                then putStrLn "Nombre de usuario copiado al portapapeles."
                else putStrLn "No se pudo copiar el nombre de usuario. Asegúrese de que 'powershell' esté disponible."
            "2" -> do
              copied <- copyToClipboard (B.unpack decryptedPassword)
              if copied
                then putStrLn "Contraseña copiada al portapapeles."
                else putStrLn "No se pudo copiar la contraseña. Asegúrese de que 'powershell' esté disponible."
            _   -> putStrLn "No se copió nada al portapapeles."
        else
          putStrLn "Operación cancelada."
    _ | null choiceStr -> return () -- Volver si se presiona Enter
    _ -> do
      putStrLn "Índice no válido."
      putStrLn "Presione Enter para continuar..."
      _ <- getLine
      viewPasswords key entries -- Llama recursivamente para nueva selección o salida

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
      putStrLn "\n=== Contraseñas Almacenadas ==="
      putStrLn "-------------------------------------------------------------"
      putStrLn "| índice | Servicio        | Usuario  | Contraseña |"
      putStrLn "-------------------------------------------------------------"
      mapM_ (\(idx, entry) ->
        putStrLn $ "| " ++ padRight 6 (show idx) ++
                  " | " ++ padRight 15 (service entry) ++
                  " | " ++ padRight 8 (maskUsername (username entry)) ++
                  " | " ++ padRight 10 "**********" ++ " |"
        ) $ zip [1 :: Integer ..] entries
      putStrLn "-------------------------------------------------------------"

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

-- Función para eliminar una contraseña
deletePassword :: String -> B.ByteString -> [PasswordEntry] -> IO [PasswordEntry]
deletePassword appUsername userPin entries = do
  putStrLn "\n=== Eliminar Contraseña ==="
  if null entries
    then do
      putStrLn "No hay contraseñas almacenadas para eliminar."
      putStrLn "Presione Enter para continuar..."
      _ <- getLine
      return entries
    else do
      -- Mostrar lista de entradas (similar a viewPasswords pero simplificado)
      putStrLn "\n=== Contraseñas Almacenadas ==="
      putStrLn "-------------------------------------------------------------"
      putStrLn "| índice | Servicio        | Usuario  | Contraseña |"
      putStrLn "-------------------------------------------------------------"
      mapM_
        ( \(idx, entry) ->
            putStrLn $
              "| "
                ++ padRight 6 (show idx)
                ++ " | "
                ++ padRight 15 (service entry)
                ++ " | "
                ++ padRight 8 (maskUsername (username entry))
                ++ " | "
                ++ padRight 10 "**********"
                ++ " |"
        )
        $ zip [1 :: Integer ..] entries
      putStrLn "-------------------------------------------------------------"

      -- Solicitar selección
      putStr "Ingrese el número de la contraseña a eliminar (o Enter para cancelar): "
      hFlush stdout
      numStr <- getLine

      case readMaybe numStr :: Maybe Integer of
        Just num | num > 0 && num <= fromIntegral (length entries) -> do
          -- Confirmar eliminación
          let entryToDelete = entries !! (fromIntegral num - 1)
          putStrLn $ "\n¿Está seguro que desea eliminar la contraseña para " ++ service entryToDelete ++ "?"
          putStr "Ingrese 's' para confirmar: "
          hFlush stdout
          confirm <- getLine

          if confirm == "s" || confirm == "S"
            then do
              let updatedEntries = take (fromIntegral num - 1) entries ++ drop (fromIntegral num) entries
              saveUserPasswords appUsername userPin updatedEntries
              putStrLn "Contraseña eliminada exitosamente."
              return updatedEntries
            else do
              putStrLn "Operación cancelada."
              return entries
        _ -> do
          putStrLn "Operación cancelada o número inválido."
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
      updatedEntries <- deletePassword appUsername userPin entries
      mainMenu key appUsername userPin updatedEntries
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
    Just (storedPinHashBase64, entries) -> do
      inputPlainPin <- getHiddenInput "Ingrese su PIN: "

      -- Hashear el PIN ingresado para comparación
      let inputPinKeyBytes = E.generateKey inputPlainPin
          inputPinHashByteString = BA.convert inputPinKeyBytes :: B.ByteString
          inputPinHashBase64 = Base64.encode inputPinHashByteString

      if inputPinHashBase64 == storedPinHashBase64
        then do
          -- Generar la clave de cifrado a partir del PIN en texto plano ingresado
          let key = E.generateKey inputPlainPin 
          putStrLn "Autenticación exitosa."
          mainMenu key appUsername_main inputPlainPin entries 
        else do
          putStrLn "PIN incorrecto. Saliendo."
    Nothing -> do
      maybeNewUserData <- createNewUser appUsername_main
      case maybeNewUserData of
        Just (newPin, newEntries) -> do
          let key = E.generateKey newPin
          mainMenu key appUsername_main newPin newEntries
        Nothing -> putStrLn "Saliendo."
