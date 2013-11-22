#!/usr/bin/env bash
mkdir ./distr
cd ./distr
wget https://opscode-omnibus-packages.s3.amazonaws.com/el/6/x86_64/chef-11.8.0-1.el6.x86_64.rpm
cd ../
git clone https://github.com/chfs-ckbks/chef-cookbooks.git
