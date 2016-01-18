# Elblog
## About

Code for my WIP personal [site](tobysmyth.uk).

Server written in Haskell using Servant.

Front end using Halogen and purescript.

Technology and function subject to change at any time.

Code quality: Dubious

## Build

### Prerequisites
* [Stack] (http://docs.haskellstack.org/en/stable/README.html)
* [npm] (https://nodejs.org/en/)

### Building

#### Front end
* `npm install` to install build tools
* `gulp` to build project.

##### Server
* `stack build` to build the server
* `stack exec elblog-server` to start the server.

Runs on port specified by `$PORT`, defaulting to 8080

