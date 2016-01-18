# Elblog
## About

Code for my WIP personal [site](http://tobysmyth.uk).

Server written in Haskell using Servant.

Front end written in Purescript using Halogen.

Technology and function subject to change at any time.

## Build

### Prerequisites
* [Stack] (http://docs.haskellstack.org/en/stable/README.html)
* [npm] (https://nodejs.org/en/)

### Building

#### Front end
* `npm install` to install the build tools.
* `gulp` to build the project.

##### Server
* `stack build` to build the server.
* `stack exec elblog-server` to start the server.

Runs on port specified by `$PORT`, defaulting to 8080.

