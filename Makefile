SHELL = /bin/bash

all: common server proxy

.PHONY: common server proxy
common:
	erlc -o common/ebin/json/ +debug_info common/json/*.erl
	erlc -o common/ebin/mslog/ +debug_info common/mslog/*.erl
	erlc -o common/ebin/ +debug_info common/*.erl
	
proxy:
	erlc -o esort_proxy/ebin +debug_info -I esort_proxy/include/ esort_proxy/src/*.erl

server:
	erlc -o esort_server/ebin +debug_info  -I esort_server/include/ esort_server/src/*.erl

release:
	erlc -o common/ebin/json/ common/json/*.erl
	erlc -o common/ebin/mslog/ common/mslog/*.erl
	erlc -o common/ebin/ common/*.erl
	erlc -o esort_proxy/ebin  -I esort_proxy/include/ esort_proxy/src/*.erl
	erlc -o esort_server/ebin -I esort_server/include/ esort_server/src/*.erl

.PHONY: clean
clean:
	-rm -f esort_proxy/ebin/*.beam
	-rm -f esort_server/ebin/*.beam
	-rm -f common/ebin/*.beam
	-rm -f common/ebin/mslog/*.beam
	-rm -f common/ebin/json/*.beam

