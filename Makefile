all: json2/json2.js jstree/jquery.jstree.js jquery/jquery.js

json2/json2.js:
	mkdir -p json2
	wget https://raw.github.com/douglascrockford/JSON-js/master/json2.js -O json2/json2.js

jstree/jquery.jstree.js:
	mkdir -p jstree
	wget http://github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip -O jstree/jstree_pre1.0_fix_1.zip
	cd jstree ; unzip jstree_pre1.0_fix_1.zip

jquery/jquery.js:
	mkdir -p jquery
	wget http://code.jquery.com/jquery-1.7.2.js -O jquery/jquery.js

README.html: README.md Makefile
	echo "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"" > $@
	echo "    \"http://www.w3.org/TR/html4/strict.dtd\">" >> $@
	echo "<html><head><title>happstack-lite tutorial</title><link rel='stylesheet' type='text/css' href='hscolour.css' ></head><body>" >> $@
	HsColour -lit -css -partial $< | markdown --html4tags >> $@
	echo "</body></html>">> $@
	validate $@

.PHONY: all