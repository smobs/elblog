# Elblog

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

