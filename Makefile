all: pjbgen mvect2 testpjb ui libs
#all: mvect2
#mvect2: mvect2.ipkg 
#	idris2 --codegen node --build mvect2.ipkg
libs: wrap_libmongoose.c wrap_libc.c sha256.c 
	#cc -shared sha256.c -o libsha256.so
	#cc mongoose.c -o mongoose.o
	cc -shared wrap_libmongoose.c wrap_libc.c mongoose.c -o libmongoose.so
	cc -shared sha256.c -o libsha256.so

mvect2: mvect2.ipkg
	#idris2 --build ws_client.ipkg
	idris2 --build mvect2.ipkg
ui: ui.ipkg
	idris2 --build ui.ipkg
pjbgen: pjbgen.ipkg
	idris2 --build pjbgen.ipkg

testpjb: pjbgen.ipkg
	idris2 --codegen chez --build testpjb.ipkg

sha:
	cc -shared sha256.c -o libsha256.so
#ws: hs_server.ipkg
#	idris --build hs_server.ipkg 

#node: Hello_WS.idr
#	idris -p contrib --codegen node Hello_WS.idr -o hello
#
#outjs: js/w3c.js
#	browserify js/w3c.js -o js/out.js

yarn: package.json yarn.lock
	../yarn2nix/result-bin/bin/yarn2nix > npm-deps.nix
	../yarn2nix/result-bin/bin/yarn2nix --template package.json  > npm-package.nix

clean:
	idris2 --clean mvect2.ipkg

