#!/bin/bash

runhaskell -i../clckwrks/ -i../clckwrks-theme-clckwrks/ -i../clckwrks-plugin-media -i../clckwrks-plugin-bugs Main.hs --http-port 8000 --jstree-path=../jstree --json2-path=../json2 --jquery-path=../jquery --bugs-data-path=../clckwrks-plugin-bugs/data "$1" "$2"