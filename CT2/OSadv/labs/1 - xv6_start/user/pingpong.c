#include "user.h"

char *get_string(int pipe[2], pde_t pid) {
  int str_size = 16, total_read = 0, read_current = 0;
  char *buf = malloc(16);
  char *final_string = malloc(str_size);

  if (!buf || !final_string) {
    fprintf(2, "Memory allocation error\n");
    exit(2);
  }

  while ((read_current = read(pipe[0], buf, 16)) > 0) {
    if (total_read + read_current >= str_size) {
      str_size *= 2;
      char *new_buf = malloc(str_size * 2 + 1);
      if (!new_buf) {
        fprintf(2, "Memory allocation error\n");
        exit(2);
      }
      memcpy(new_buf, final_string, total_read);
      free(final_string);
      final_string = new_buf;
    }
    memcpy(final_string + total_read, buf, read_current);
    total_read += read_current;
  }

  if (read_current < 0) {
    fprintf(2, "Read error\n");
    exit(1);
  }

  final_string[total_read] = '\0';
  free(buf);

  return final_string;
}

int main(int argc, char *argv[]) {
  int pipe_parent[2], pipe_child[2];
  char *ping = "ping", *pong = "pong";
  pde_t pid;

  if (pipe(pipe_parent) == -1 || pipe(pipe_child) == -1) {
    fprintf(2, "Pipe creation failed\n");
    exit(1);
  }

  pid = fork();

  if (pid < 0) {
    fprintf(2, "Fork failed\n");
    exit(1);
  } else if (pid == 0) {
    close(pipe_parent[0]);
    close(pipe_child[1]);

    printf("%d: got %s\n", getpid(), get_string(pipe_child, pid));
    write(pipe_parent[1], pong, strlen(pong) + 1);
    close(pipe_parent[1]);
    close(pipe_child[0]);

    exit(0);
  } else {
    close(pipe_parent[1]);
    close(pipe_child[0]);

    write(pipe_child[1], ping, strlen(ping) + 1);
    close(pipe_child[1]);

    printf("%d: got %s\n", getpid(), get_string(pipe_parent, pid));
    close(pipe_parent[0]);

    exit(0);
  }
}