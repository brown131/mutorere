#!/bin/sh
# Execute the app inside a running dev container

# This may require enabling iglx in XQuartz first:
# defaults write org.xquartz.X11 enable_iglx -bool true  

CONTAINER=$(docker ps | grep 'vsc-mutorere' | awk '$2~"vsc-mutorere" {print $1}')
WORKSPACE=/workspaces/mutorere
APP=dist-newstyle/build/x86_64-linux/ghc-8.10.7/mutorere-1.0.0/build/mutorere/mutorere
IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')

xhost + $IP
docker exec -ti -e DISPLAY=$IP:0 -u vscode $CONTAINER $WORKSPACE/$APP
