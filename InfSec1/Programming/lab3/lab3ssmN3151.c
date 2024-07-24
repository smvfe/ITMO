#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <errno.h>

enum exit_codes {
  OK = 0,
  WRONG_FLAG = 1,
  FILE_ERROR = 2,
  TYPE_ERROR = 3,
  MALLOC_ERROR = 4
};

typedef struct {
  bool n;
  bool c;
  unsigned long f;
  unsigned long t;
} flags;

flags flag = {false, false, 1, (unsigned long)-1};
FILE *input_file = NULL, *output_file = NULL;

bool is_num(char *str) {
  char *end_of_str;
  long val = strtol(str, &end_of_str, 10);

  if (*end_of_str == '\0' && val >= 0)
    return true;

  return false;
}

bool validation(char *pattern, char *str) {
  size_t len = strlen(pattern);
  for (size_t i = 0; i < len; i++) {
    if (pattern[i] == 'd' && isdigit(str[i]) == 0)
      return false;
    else if (pattern[i] != str[i])
      return false;
  }
  return true;
}

bool is_date(char *str) {
  if (!validation("dd:dd:dd dd.dd.dddd", str))
    return false;

  int day, month, year;
  int seconds, minutes, hours;
  sscanf(str, "%d:%d:%d %d.%d.%d", &hours, &minutes, &seconds, &day, &month, &year);

  if (day < 1 || day > 31 || month < 1 || month > 12 || year < 1 || year > 9999 || hours > 23 || minutes > 59 || seconds > 59)
    return false;
  if (day > 30 && (month == 4 || month == 6 || month == 9 || month == 11 || month == 2))
    return false;
  if (month == 2 && day > 29)
    return false;
  if (month == 2 && day == 29 && !(year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)))
    return false;

  return true;
}

void flag_check(int argc, char **argv) {
  char *temp = (char *)malloc(256);

  if (!temp) {
    perror("Не удалось выделить память");
    exit(MALLOC_ERROR);
  }

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-v") == 0) {
      printf("Мочеков Семён Сергеевич, гр. N3151\nВариант: 8-2-3-3\n");
      exit(OK);
    }
    else if (strcmp(argv[i], "-n") == 0) {
      flag.n = true;
    }
    else if (strcmp(argv[i], "-c") == 0) {
      flag.c = true;
    }
    else if (argv[i][0] == '-' && argv[i][1] == 'f' && argv[i][2] == '=') {
      strncpy(temp, argv[i] + 3, strlen(argv[i]) - 3);
      if (is_num(temp)) {
        flag.f = strtol(temp, NULL, 10);
      }
      else {
        perror("Неверный тип данных для -f.\n");
        exit(TYPE_ERROR);
      }
    }
    else if (argv[i][0] == '-' && argv[i][1] == 't' && argv[i][2] == '=') {
      strncpy(temp, argv[i] + 3, strlen(argv[i]) - 3);
      if (is_num(temp)) {
        flag.t = strtol(temp, NULL, 10);
      }
      else {
        perror("Неверный тип данных для -f.\n");
        exit(TYPE_ERROR);
      }
    }
    else if (input_file == NULL) {
      input_file = fopen(argv[i], "r");
      if (input_file == NULL) {
        perror("Не удалось открыть файл.\nПроверьте корректность указанного пути");
        exit(FILE_ERROR);
      }
    }
    else if (output_file == NULL) {
      output_file = fopen(argv[i], "w+");
      if (output_file == NULL) {
        perror("Некая проблема c выводным файлом.\nПередайте это программисту");
        exit(FILE_ERROR);
      }
    }
    else {
      printf("Неподходящие аргументы запуска программы!\n");
      exit(WRONG_FLAG);
    }
  }
  free(temp);
}

int main(int argc, char **argv) {
  flag_check(argc, argv);
  
  if (input_file == NULL) {
    input_file = fopen("tmp", "w+");
    if (input_file == NULL) {
      perror("Ошибка при создании временного файла");
      exit(FILE_ERROR);
    }
    int chars;
    while ((chars = getchar()) != EOF) {
      fputc(chars, input_file);
    }
    rewind(input_file);
  }

  unsigned long long file_size;
  fseek(input_file, 0, SEEK_END);
  file_size = ftell(input_file) + 1;
  fseek(input_file != NULL ? input_file : stdin, 0, SEEK_SET);

  char *buffer = (char *)malloc(file_size), *pattern_buffer = (char *)malloc(19);
  if (!buffer || !pattern_buffer) {
    perror("Не удалось выделить память");
    exit(MALLOC_ERROR);
  }

  fread(buffer, 1, file_size, input_file);
  unsigned long objects = 0;
  unsigned long long  i = 0;

  while (i < file_size) {
    strncpy(pattern_buffer, buffer + i, 19);
    if (!flag.n && pattern_buffer[8] == '\n') {
      pattern_buffer[8] = ' ';
    }
    if (is_date(pattern_buffer) && (++objects >= flag.f ) &&(flag.t == (unsigned long long)-1 || objects <= flag.t)) {
      strncpy(pattern_buffer, buffer + i, 19);
      fprintf(output_file == NULL ? stdout : output_file, flag.c ? "\e[33m%s\e[m" : "`%s`", pattern_buffer);
      i+=18;
    }
    else {
      fprintf(output_file == NULL ? stdout : output_file, "%c", buffer[i]);
    }
    i++;
  }

  free(buffer);
  free(pattern_buffer);
  if (input_file != NULL) {
    fclose(input_file);
    remove("tmp");
  }
  if (output_file != NULL){
    fclose(output_file);
  }

  return 0;
}       