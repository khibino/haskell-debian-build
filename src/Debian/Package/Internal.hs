
module Debian.Package.Internal
       ( tarGz ) where

import System.FilePath ((<.>))


tarGz :: String
tarGz =  "tar" <.> "gz"
