#!/bin/sh
erl -pa ../noiseling/ebin ../noiseling/deps/*/ebin -s noiseling -network_device wlan0 -port 8080
