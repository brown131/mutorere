open -a XQuartz
IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
xhost + $IP
CONT=zenhaskell/foundation:latest
CMD=nixshell
docker run -it -w /mutorere -v $(pwd):/mutorere -v /private/tmp:/private/tmp -e DISPLAY=$IP:0 $CONT $CMD
# nix-env -i wget
# cabal update
# cabal run

