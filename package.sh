#!/bin/zsh
# Package mutorere for docker distribution

IMAGE=brown131/mutorere

docker images | grep $IMAGEd
if [ $? -ne 0 ]
then docker build . -t $IMAGE
fi

docker save $IMAGE | gzip > ${IMAGE/\//_}.tgz
