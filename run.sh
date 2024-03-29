#!/bin/sh
# Run the app in a dev container

# This may require enabling iglx in XQuartz first:
# defaults write org.xquartz.X11 enable_iglx -bool true  

IMAGE=$(docker images | grep 'vsc-mutorere' | awk '$1~"vsc-mutorere" {print $1}')
WORKSPACE=/workspaces/mutorere
APP=dist-newstyle/build/x86_64-linux/ghc-8.10.7/mutorere-1.0.0/build/mutorere/mutorere
IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')

xhost + $IP
docker run -ti --name mutorere --rm -e DISPLAY=$IP:0 -u vscode -v $(pwd):$WORKSPACE $IMAGE $WORKSPACE/$APP

