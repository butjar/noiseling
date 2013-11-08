#!/bin/sh
erl -pa ../webling/ebin ../webling/deps/*/ebin -s streamling -sname client
