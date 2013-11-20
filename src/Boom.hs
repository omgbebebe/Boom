{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

--import Boom.Helpers
import Shelly
import Data.Text as T
import qualified Data.List as L
import Control.Monad
import Control.Concurrent

import Boom.Helpers
import Boom.Config as BC
default(T.Text)

main = shelly $ verbosely $ do
  echo "Prepare for BOOM!:)"
  echo_n "Loading env configuration..."
  conf <- loadConf
  nodes <- loadNodes conf
  echo "[OK]"
  echo "Preparing nodes."
  forM_ nodes (\n -> liftIO $ forkChild $ recreate_node conf n)
  liftIO $ waitForChildren
  echo "Preparation is done."
  echo "You are Boomed!:)"
{--
  recreate_node ""
  destroy_instances conf
  destroy_volumes conf
  create_volumes conf
  create_instances conf
  start_instances conf

  update_cookbooks conf
  chef_update conf
  deploy_nodes conf
--}


update_cookbooks conf = shelly $ silently $ do
  cd $ BC.cookbookPath conf
  git_checkout $ commitHash conf
  git_submodule "init"
  git_submodule "sync"
  git_submodule "update"
  where git_checkout hash = run_ "git" ["checkout", hash]
        git_submodule cmd = run_ "git" ["submodule", cmd]

recreate_node :: Config -> Node -> IO ()
recreate_node conf n = do
  shelly $ recreate_node' conf n
  return ()

recreate_node' :: Config -> Node -> Sh ()
recreate_node' conf node = shelly $ do
  echo $ "Recreating node: " `append` (nodeName node)
  echo "Destroying old instance"
  destroy_instance node
  destroy_volume conf node
  create_volume conf node
  node_def <- create_node_def conf node
  start_instance node_def
  config_instance conf node
  echo $ (nodeName node) `append` " recreated."

destroy_instance :: Node -> Sh ()
destroy_instance Node{..} = shelly $ silently $ do
  setenv "LANG" "C"
  is <- fmap (L.init . T.lines) $ sudo "virsh" ["list","--all", "--name"]
  let inst = L.filter (==nodeName) is --L.map (L.drop 0 . T.words) is
  forM_ inst (\x -> sudo_ "virsh" ["destroy", x])



destroy_volume Config{..} node@Node{..} = shelly $ silently $ do
  vs <- fmap (L.drop 1 . T.lines) $ sudo "lvs" []
  let volumes = L.filter (==nodeName) $ volNames
      volNames = L.map (L.head . T.words) vs
  forM_ volumes (\x -> sudo_ "lvremove" [volPath, "-f"])
    where volPath = toTextIgnore $ (fromText "/dev") </> vgName </> nodeName



create_volume Config{..} Node{..} = shelly $ silently $ do
  sudo_ "lvcreate" ["-L", volSize, "-s", "-n", nodeName, tmplPath]
  where tmplPath = toTextIgnore $ (fromText "/dev") </> vgName </> nodeVTempl

create_node_def Config{..} Node{..} = shelly $ silently $ do
  tmpl <- readfile $ fromText nodeITempl
  let tmpl' = T.replace "node_name" nodeName tmpl
      tmpl'' = T.replace "node_volume" volPath tmpl'
      resFile = (fromText ".") </> nodeName <.> "xml"
      volPath = toTextIgnore $ (fromText "/dev") </> vgName </> nodeName
  writefile resFile tmpl''
  return resFile

start_instance nodeDef = shelly $ silently $ do
  sudo_ "virsh" ["create", toTextIgnore nodeDef]



config_instance :: Config -> Node -> Sh ()
config_instance conf@Config{..} node@Node{..} = shelly $ silently $ do
  mac <- get_node_mac node
  ip <- wait_node_ip conf node
  wait_for_port ip 22
  conf_by_ssh ip keyPath script

  
sudo :: Text -> [Text] -> Sh (Text)
sudo cmd args = run "sudo" (cmd:args)

sudo_ :: Text -> [Text] -> Sh ()
sudo_ cmd args = run_ "sudo" (cmd:args)
                          
