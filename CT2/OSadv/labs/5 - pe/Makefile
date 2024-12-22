CC = gcc
CFLAGS = -Wall -Wextra
TARGET = pe-parser

all: $(TARGET)

$(TARGET): $(TARGET).c
	$(CC) $(CFLAGS) -o $(TARGET) $(TARGET).c

validation-pe-tests: all
	python3 -m tests ValidatingPeTestCases -f

import-dll-tests: all
	python3 -m tests ImportDllTestCases -f

import-function-tests: all
	python3 -m tests ImportFunctionTestCases -f
	
export-function-tests: all
	python3 -m tests ExportFunctionTestCases -f


clean:
	rm -f $(TARGET) $(TARGET).o

test1: all
	./pe-parser is-pe ./examples/1/1.exe
test2: all
	./pe-parser import-functions ./examples/2/2.exe
test3: export-function-tests
	./pe-parser export-functions ./examples/3/3.dll
test4: all validation-pe-tests import-dll-tests import-function-tests export-function-tests