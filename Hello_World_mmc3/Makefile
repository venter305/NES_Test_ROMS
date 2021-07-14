hello_world.nes: hello_world.o
	/opt/cc65/bin/ld65 hello_world.o -t nes -o hello_world.nes

hello_world.o: hello_world.s
	/opt/cc65/bin/ca65 hello_world.s -o hello_world.o


clean:
	rm -f hello_world.o hello_world.nes
