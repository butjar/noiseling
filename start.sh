#!/bin/sh
erl -smp disable -pa ../noiseling/ebin ../noiseling/deps/*/ebin -s noiseling -port 8080
