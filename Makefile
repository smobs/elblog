all: setup build

setup: 
	stack setup

build: server-build	ps-build

server-build: 
	stack build

ps-build: 
	stack exec psgen