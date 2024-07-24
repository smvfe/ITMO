CXX = gcc
WORKFLAGS = -Wall -Wextra -Werror -O

SRCS = lab4
NAME = lab4

all: $(SRCS)

$(SRCS): $(SRCS).c
	$(CXX) $(WORKFLAGS) -o $(NAME) $(SRCS).c

clean:
	rm -f $(NAME).exe
	rm -f *~