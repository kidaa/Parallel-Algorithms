# 15-210 CilkLab

OBJDIR = objs
OBJS = $(OBJDIR)/main.o $(OBJDIR)/cilkStars.o $(OBJDIR)/serialStars.o 
EXECUTABLE = cilkStars

CC = g++
DEBUG = -g
CFLAGS = -fcilkplus -lcilkrts -std=c++11 -m64 -Werror -O2 -c $(DEBUG)
LFLAGS = -fcilkplus -lcilkrts -std=c++11 -m64 -Werror $(DEBUG)

default: $(EXECUTABLE)

dirs:
	mkdir -p $(OBJDIR)/

$(EXECUTABLE): dirs $(OBJS)
	$(CC) $(LFLAGS) $(OBJS) -o $@

$(OBJDIR)/main.o: main.cpp cilkStars.h serialStars.h util.h
	$(CC) $(CFLAGS) main.cpp -o $@

$(OBJDIR)/cilkStars.o: cilkStars.h cilkStars.cpp util.h
	$(CC) $(CFLAGS) cilkStars.cpp -o $@

$(OBJDIR)/serialStars.o: serialStars.h serialStars.cpp util.h
	$(CC) $(CFLAGS) serialStars.cpp -o $@

$(OBJDIR)/util.o: util.h util.cpp
	$(CC) $(CFLAGS) util.cpp -o $@

clean:
	rm -rf $(OBJDIR)
	rm -f $(EXECUTABLE)
