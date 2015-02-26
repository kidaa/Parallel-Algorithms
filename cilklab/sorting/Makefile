# 15-210 CilkLab

OBJDIR = objs
QSORTOBJS = $(OBJDIR)/quickSort.o $(OBJDIR)/qsortTime.o
SSORTOBJS = $(OBJDIR)/sampleSort.o $(OBJDIR)/quickSort.o $(OBJDIR)/ssortTime.o

CC = g++
DEBUG = -g
CFLAGS = -fcilkplus -lcilkrts -m64 -O3 -c $(DEBUG)
LFLAGS = -fcilkplus -lcilkrts -m64 -O3 $(DEBUG)

default: dirs qsortTime ssortTime

dirs:
	mkdir -p $(OBJDIR)/


qsortTime: $(QSORTOBJS)
	$(CC) $(LFLAGS) $(QSORTOBJS) -o $@

$(OBJDIR)/quickSort.o: quickSort.cpp quickSort.h
	$(CC) $(CFLAGS) quickSort.cpp -o $@

$(OBJDIR)/qsortTime.o: sortTime.cpp
	$(CC) $(CFLAGS) -DserialSort=quickSortSerial -DparallelSort=quickSort sortTime.cpp -o $@


ssortTime: $(SSORTOBJS)
	$(CC) $(LFLAGS) $(SSORTOBJS) -o $@

$(OBJDIR)/sampleSort.o: sampleSort.cpp quickSort.h
	$(CC) $(CFLAGS) sampleSort.cpp -o $@

$(OBJDIR)/ssortTime.o: sortTime.cpp
	$(CC) $(CFLAGS) -DserialSort=sampleSortSerial -DparallelSort=sampleSort sortTime.cpp -o $@


clean:
	rm -rf $(OBJDIR)
	rm -f ssortTime qsortTime
