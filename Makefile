SOURCES = dynvector/dynvector.o hypermath/hypermath.o core.o
INCLUDE = -I./hypermath -I./dynvector -I./include
LIBS    = `pkg-config --libs --cflags SDL2_ttf sdl2 SDL2_image glew` -lm
CFLAGS  = -g3 -DDEBUG -Wall -pedantic -std=c11 -fPIC
TARGET  = build/libcoati.so
PREFIX  = /usr/local

all: $(SOURCES)
	mkdir -p build
	$(CC) -shared  $(CFLAGS) $(SOURCES) $(INCLUDE) $(LIBS) -o $(TARGET)

core.o: core.c include/core.h
	$(CC) $(CFLAGS) $(INCLUDE) -c $< -o $@

dynvector/dynvector.o: dynvector/dynvector.c dynvector/dynvector.h
	$(CC) $(CFLAGS) $(INCLUDE) -c $< -o $@

hypermath/hypermath.o: hypermath/hypermath.c hypermath/hypermath.h
	$(CC) $(CFLAGS) $(INCLUDE) -c $< -o $@

install: $(TARGET)
	install $(TARGET) $(PREFIX)/lib
	cp -r include/ $(PREFIX)/include/coati
	ldconfig $(PREFIX)/lib

uninstall:
	rm -f $(PREFIX)/lib/$(TARGET)
	rm -fr $(PREFIX)/include/coati

clean:
	rm -f $(SOURCES)
	rm -f $(TARGET)
