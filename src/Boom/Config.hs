{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Boom.Config where

import Prelude hiding (FilePath)
import Shelly
import Data.Text as T
import Data.List as DL
import Data.Default
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad
import System.Environment

f1 = do
  env <- getEnvironment
  let c = DL.foldl (\a (e,v) -> fromEnv a (T.pack e) (T.pack v)) def env
  print c

fromEnv c e v =
  let vp = fromText v in
  case e of
    "B_COMMIT_HASH"       -> c{commitHash=v}
    "B_COOKBOOK_PATH"     -> c{cookbookPath=vp}
    "B_VG_NAME"           -> c{vgName=v}
    "B_LV_SNAP_SIZE"      -> c{lvSnapSize=v}
    "B_CHEF_HOSTNAME"     -> c{chefHostname=v}
    "B_CHEF_ADDR"         -> c{chefAddr=v}
    "B_CHEF_CLIENT_RPM"   -> c{chefClientRpm=vp}
    "B_ENV_FILE"          -> c{envFile=vp}
    "B_ENV_NAME"          -> c{envName=v}
    "B_DEF_GW"            -> c{defaultGw=v}
    "B_HTTP_PROXY"        -> c{httpProxy=v}
    "B_HTTPS_PROXY"       -> c{httpsProxy=v}
    "B_YUM_PROXY"         -> c{yumProxy=v}
    "B_SSH_KEY"           -> c{sshKey=vp}
    "B_EXT_IF"            -> c{extIf=v}
    "B_INT_IF"            -> c{intIf=v}
    "B_NODES_FILE"        -> c{nodesFile=vp}
    "B_LEASES_FILE"       -> c{leasesFile=vp}
    "B_DISTR_PATH"        -> c{distrPath=vp}
    _ -> c


data Config = Config{ commitHash    :: Text
                    , cookbookPath  :: FilePath
                    , vgName        :: Text
                    , lvSnapSize    :: Text
                    , chefHostname  :: Text
                    , chefAddr      :: Text
                    , chefClientRpm :: FilePath
                    , envFile       :: FilePath
                    , envName       :: Text
                    , defaultGw     :: Text
                    , httpProxy     :: Text
                    , httpsProxy    :: Text
                    , yumProxy      :: Text
                    , sshKey        :: FilePath
                    , extIf         :: Text
                    , intIf         :: Text
                    , nodesFile     :: FilePath
                    , leasesFile    :: FilePath
                    , distrPath     :: FilePath
                    }
            deriving (Eq, Show)

instance Default Config where
  def = Config{ commitHash      = "33c771c2ebe4b4c03d50e62fadfc5f0445cead9b"
              , cookbookPath    = "./chef-cookbooks"
              , vgName          = "raid1"
              , lvSnapSize      = "1G"
              , chefHostname    = "chef.wd.com"
              , chefAddr        = "10.0.104.2"
              , chefClientRpm   = "chef-11.8.0-1.el6.x86_64.rpm"
              , envFile         = "./wdm_ha.json"
              , envName         = "wdm_ha"
              , defaultGw       = "192.168.122.1"
              , httpProxy       = "http://10.0.104.1:3128"
              , httpsProxy      = "http://10.0.104.1:3128"
              , yumProxy        = "http://10.0.104.1:3128"
              , sshKey          = "/home/scor/.ssh/id_rsa"
              , extIf           = "eth0"
              , intIf           = "eth1"
              , nodesFile       = "./vms.org"
              , leasesFile      = "/var/lib/libvirt/dnsmasq/default.leases"
              , distrPath       = "./distr"
              }

loadConf :: Sh (Config)
loadConf = shelly $ do
  env <- liftIO $ getEnvironment
  let c = DL.foldl (\a (e,v) -> fromEnv a (T.pack e) (T.pack v)) def env
  return c

data Node = Node{ nodeName   :: Text
                , nodeOctet  :: Text
                , nodeRole   :: Text
                , nodeITempl :: Text
                , nodeVTempl :: Text
                , volSize    :: Text
                , controlIP  :: Maybe Text
                }
          deriving (Eq, Show)

type Nodes = [Node]

loadNodes :: Config -> Sh (Nodes)
loadNodes Config{..} = shelly $ do
  cont <- readfile $ nodesFile
  let ls  = DL.drop 2 $ T.lines cont
      vms = DL.map (\v -> let v' = T.words $ T.filter (/='|') v
                              [n,o,r,it,vt] = v'
                          in Node n o r it vt lvSnapSize Nothing
                   ) ls
  return vms
