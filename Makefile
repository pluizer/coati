SOURCES = dynvector/dynvector.c hypermath/hypermath.c coati.c
INCLUDE = -I./hypermath -I./dynvector
LIBS    = `pkg-config --libs --cflags SDL2_ttf sdl2 SDL2_image glew` -lm
CFLAGS  = -g3 -DDEBUG -Wall -pedantic -std=c11
TARGET  = build/libcoati.so
PREFIX  = /usr/local

all:
	mkdir -p build
	$(CC) -shared -fPIC $(CFLAGS) $(SOURCES) $(INCLUDE) $(LIBS) -o $(TARGET)

install: $(TARGET)
	install $(TARGET) $(PREFIX)/lib
	install coati.h $(PREFIX)/include
	ldconfig $(PREFIX)/lib

uninstall:
	rm -f $(PREFIX)/lib/$(TARGET)
	rm -f $(PREFIX)/include/coati.h

clean:
	rm -f $(TARGET)
