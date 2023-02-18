##
# Project Title
#
# @file
# @version 0.1

build:
	stack build

all: build
	stack run -- --hello vismay

# end
