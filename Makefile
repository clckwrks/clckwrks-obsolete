all: json2/json2.js jstree/jquery.jstree.js

json2/json2.js:
	mkdir -p json2
	wget https://raw.github.com/douglascrockford/JSON-js/master/json2.js -O json2/json2.js

jstree/jquery.jstree.js:
	mkdir -p jstree
	wget http://github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip -O jstree/jstree_pre1.0_fix_1.zip
	cd jstree ; unzip jstree_pre1.0_fix_1.zip



.PHONY: all