{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_PasswordManager (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "PasswordManager"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A password manager application"
copyright :: String
copyright = "2025 Author name here"
homepage :: String
homepage = "https://github.com/githubuser/PasswordManager#readme"
