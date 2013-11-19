###-------------------------------------------------------------------
### @author Martin Fleischer <butjar@butjar-ThinkPad-X1-Carbon>
### @copyright (C) 2013, Martin Fleischer
###-------------------------------------------------------------------
#!/bin/sh
erl -smp disable -pa ../noiseling/ebin ../noiseling/deps/*/ebin -s noiseling -port 8080
