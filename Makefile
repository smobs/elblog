all: setup build

setup: 
	stack setup

build: server-build	ps-build

server-build: install

ps-build: 
	stack exec psgen

install:
	stack build --fast --copy-bins