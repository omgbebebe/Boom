{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Prelude hiding (FilePath)
import Shelly
import Data.Text as T
import qualified Data.List as L
import Control.Monad
import Control.Concurrent
import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)

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
  forM_ nodes (\n -> liftIO $ forkChild $ do
                     node <- recreate_node conf n
                     provision_node conf node
              )
  liftIO $ waitForChildren
  echo "Preparation is done."
  waitForSolr $ T.concat ["chef_environment:", envName conf, " AND role:ha-controller1"]
  let ssh_key = toTextIgnore (sshKey conf)
  escaping False $
    knife_ "ssh" [T.concat ["\"chef_environment:",envName conf," AND role:ha-controller1\""]
                 ,"chef-client", "-x", "root", "-i", ssh_key]
  escaping False $
    knife_ "ssh" [T.concat ["\"chef_environment:",envName conf," AND (role:ha-controller2 OR role:single-compute)\""]
                 ,"chef-client", "-x", "root", "-i", ssh_key]
  escaping False $
    knife_ "ssh" [T.concat ["\"chef_environment:",envName conf," AND role:ha-controller1\""]
                 ,"chef-client", "-x", "root", "-i", ssh_key]
  echo "You are Boomed!:)"


update_cookbooks conf = shelly $ silently $ do
  cd $ BC.cookbookPath conf
  git_checkout $ commitHash conf
  git_submodule "init"
  git_submodule "sync"
  git_submodule "update"
  where git_checkout hash = run_ "git" ["checkout", hash]
        git_submodule cmd = run_ "git" ["submodule", cmd]

update_chef_env :: Config -> Sh ()
update_chef_env conf@Config{..} = shelly $ do
  knife_ "environment" ["from", "file", toTextIgnore envFile]
  knife_ "role" ["from", "file", (toTextIgnore cookbookPath) `append` "roles/*.rb"]

provision_node :: Config -> Node -> IO ()
provision_node conf node = do
  shelly $ provision_node' conf node

provision_node' :: Config -> Node -> Sh ()
provision_node' conf@Config{..} node@Node{..} = shelly $ do
  case controlIP of
    Nothing -> errorExit "Node IP is not set"
    Just ip -> do
      echo $ "Prov: Node IP " `append` ip
      mayFail $ knife_ "node" ["delete", nodeName, "-y"]
      mayFail $ knife_ "client" ["delete", nodeName, "-y"]
      knife_ "bootstrap" [ip,"-x","root","-E",envName,"-i",toTextIgnore sshKey,"-r",roles]
      knife_ "node" ["run_list","add", nodeName, T.concat ["role[",nodeRole,"]"]]
      where roles = if isInfixOf "controller" (nodeRole)
                    then T.concat ["role[base]",",","recipe[build-essential]"]
                    else "role[base]"


recreate_node :: Config -> Node -> IO (Node)
recreate_node conf n = do
  node <- shelly $ recreate_node' conf n
  return node


recreate_node' :: Config -> Node -> Sh (Node)
recreate_node' conf node = shelly $ do
  echo $ "Recreating node: " `append` (nodeName node)
  echo "Destroying old instance"
  destroy_instance node
  destroy_volume conf node
  create_volume conf node
  node_def <- create_node_def conf node
  start_instance node_def
  ip <- config_instance conf node
  echo $ (nodeName node) `append` " recreated."
  return $ node{controlIP=Just ip}

destroy_instance :: Node -> Sh ()
destroy_instance Node{..} = shelly $ silently $ do
  setenv "LANG" "C"
  is <- fmap (L.init . T.lines) $ sudo "virsh" ["list","--all", "--name"]
  let inst = L.filter (==nodeName) is
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



config_instance :: Config -> Node -> Sh (Text)
config_instance conf@Config{..} node@Node{..} = shelly $ do
  mac <- get_node_mac node
  ip <- liftIO $ wait_node_ip (T.unpack $ toTextIgnore leasesFile) mac
  echo mac
  echo $ "IP: " `append` ip
  escaping False $ sudo_ "sed" [T.concat ["-i '/.*",nodeName,"$/d' /etc/hosts"]]
  add_to_hosts ip nodeName
  echo "Waiting for SSH reply"
  liftIO $ wait_for_port ip 22
  script <- render_script conf node
  conf_by_ssh ip conf script
  return ip

conf_by_ssh :: Text -> Config -> Text -> Sh ()
conf_by_ssh ip Config{..} script = shelly $ do
  echo "conf by ssh"
  scp_ (distrPath </> chefClientRpm) ip
  ssh_ ip script
  where ssh addr cmd = run "ssh" $ sshOpts ++ [addr,cmd]
        ssh_ addr cmd = run_ "ssh" $ sshOpts ++ [addr,cmd]
        scp_ file ip = run_ "scp" $ scpOpts ++ [toTextIgnore file,"root@" `append` ip `append` ":/tmp/"]
        sshOpts = ["-i",toTextIgnore sshKey
                  ,"-l","root"
                  ,"-o","StrictHostKeyChecking=no"
                  ,"-o","UserKnownHostsFile=/dev/null"]
        scpOpts = ["-i",toTextIgnore sshKey
                  ,"-o","StrictHostKeyChecking=no"
                  ,"-o","UserKnownHostsFile=/dev/null"]

  
get_node_mac :: Node -> Sh (Text)
get_node_mac Node{..} = shelly $ silently $ do
  xml <- sudo "virsh" ["dumpxml",nodeName]
  let mac = mac_from_xml xml
  return mac

sudo :: Text -> [Text] -> Sh (Text)
sudo cmd args = run "sudo" (cmd:args)

sudo_ :: Text -> [Text] -> Sh ()
sudo_ cmd args = run_ "sudo" (cmd:args)
                          
mayFail cmd = catchany_sh cmd $ \_ -> echo "safe fail"



render_script :: Config -> Node -> Sh (Text)
render_script conf@Config{..} node@Node{..} = shelly $ do
  let
    res = renderHtml [shamlet|
echo "#{chefAddr}  ${chefHostname}" >> /etc/hosts;
echo "10.0.104.#{nodeOctet}  #{nodeName}" >> /etc/hosts;
sed -i 's/HOSTNAME=.*/HOSTNAME=#{nodeName}/' /etc/sysconfig/network;
hostname #{nodeName};
echo "GATEWAY=#{defaultGw}" >> /etc/sysconfig/network;
echo -en "DEVICE=#{intIf}\nTYPE=Ethernet\nONBOOT=yes\nNM_CONTROLLED=yes\nBOOTPROTO=none" > /etc/sysconfig/network-scripts/ifcfg-#{intIf};
echo -en "VLAN=yes\nDEVICE=#{intIf}.100\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.100.#{nodeOctet}\nNETMASK=255.255.255.0" > /etc/sysconfig/network-scripts/ifcfg-#{intIf}.100;
echo -en "VLAN=yes\nDEVICE=#{intIf}.101\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.101.#{nodeOctet}\nNETMASK=255.255.255.0" > /etc/sysconfig/network-scripts/ifcfg-#{intIf}.101;
echo -en "VLAN=yes\nDEVICE=#{intIf}.102\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.102.#{nodeOctet}\nNETMASK=255.255.255.0" > /etc/sysconfig/network-scripts/ifcfg-#{intIf}.102;
echo -en "VLAN=yes\nDEVICE=#{intIf}.103\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.103.#{nodeOctet}\nNETMASK=255.255.255.0" > /etc/sysconfig/network-scripts/ifcfg-#{intIf}.103;
echo -en "VLAN=yes\nDEVICE=#{intIf}.104\nTYPE=Ethernet\nONBOOT=yes\nIPADDR=10.0.104.#{nodeOctet}\nNETMASK=255.255.255.0" > /etc/sysconfig/network-scripts/ifcfg-#{intIf}.104;
service network restart;
echo "proxy=#{yumProxy}" >> /etc/yum.conf;
echo "http_proxy=#{httpProxy}" >> /root/.bash_profile;
echo "https_proxy=#{httpsProxy}" >> /root/.bash_profile;
echo "export http_proxy" >> /root/.bash_profile;
echo "export https_proxy" >> /root/.bash_profile;
rpm -Uvhi /tmp/#{toTextIgnore chefClientRpm};
|]

  return $ T.pack res



knife_ cmd args = run_ "knife" (cmd:args)


add_to_hosts :: Text -> Text -> Sh ()
add_to_hosts ip name = shelly $ do
  run "echo" [ip, "    ", name] -|- sudo_ "tee" ["-a", "/etc/hosts"]


waitForSolr :: Text -> Sh ()
waitForSolr req = shelly $ do
  res <- run "knife" ["search", "node", req]
  liftIO $ threadDelay (2 * 1000000)
  if isInfixOf "0 items found" res
    then waitForSolr req
    else return ()
