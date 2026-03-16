#!/bin/bash

echo "Starting IHaskell notebook..."
echo "This will pull the Docker image and start Jupyter Lab with IHaskell kernel"
echo "Current directory will be mounted as /home/jovyan/pwd in the container"
echo ""

docker run --rm \
  -p 8888:8888 \
  -v "$PWD:/home/jovyan/pwd" \
  --name ihaskell_notebook \
  ghcr.io/ihaskell/ihaskell-notebook:master \
  jupyter lab \
    --ServerApp.token='' \
    --ip=0.0.0.0 \
    --allow-root