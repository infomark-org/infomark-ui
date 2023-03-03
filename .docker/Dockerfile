FROM node:8
RUN apt update && apt install -y git && rm -rf /var/lib/apt/lists/*
RUN npm install -g elm-github-install create-elm-app@3.0.6 --unsafe-perm=true
WORKDIR /source
CMD elm-app build

