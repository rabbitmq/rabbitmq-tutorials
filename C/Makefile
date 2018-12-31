# building all .c files in the directory
# each one into its own target

CXX ?= gcc

CXXFLAGS ?= -Wall -Werror

LIBS = -lrabbitmq

.PHONY: all clean

SRCS = $(wildcard *.c)

PROGS = $(patsubst %.c,%,$(SRCS))

all: $(PROGS)

%: %.c
	$(CXX) $(CXXFLAGS) -o $@ $< $(LIBS)

clean: 
	rm -f $(PROGS)
