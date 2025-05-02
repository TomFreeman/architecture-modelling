#! /bin/bash

dotnet tool restore

# Python diagrams
sudo apt update
sudo apt install graphviz -y
pip install Diagrams
