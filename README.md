# InfoMark-Frontend

[![Build Status](https://ci.patwie.com/api/badges/infomark-org/infomark-ui/status.svg)](http://ci.patwie.com/infomark-org/infomark-ui)

InfoMark is a CI inspired online course management system. The goal is to achieve auto testing of exercises/homework using unit tests to ease the task of TAs.
This repo hosts the frontend of the application. It is written in [Elm](https://elm-lang.org/), a functional
frontend language which compiles to JavaScript. The application is just a single page which uses fragments for
routing. So the server only needs to distribute the static HTML page and the REST Api which is used to
interact with the server. The [API](https://infomark.org/swagger/) is defined using [Swagger](https://swagger.io/).

The server is implemented in [Go](https://golang.org/) and is available [here](https://github.com/infomark-org/infomark-backend).

# Building

Follow these [Instructions](https://github.com/infomark-org/infomark-ui/wiki/Installation) to set up the development environment and get an overview of the included build scripts.
