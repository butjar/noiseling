#!/bin/sh
erl -pa ../streamling/ebin ../streamling/deps/*/ebin -s streamling -sname client
