#include "parspack/lib.h"

int main(int argc, char **argv) {
  input_check(argc);

  FILE *file = fopen(argv[2], "r");

  if (strcmp(argv[1], "is-pe") == 0) 
    is_pe(file);

  if (strcmp(argv[1], "import-functions") == 0)
    import_functions(file);

  if (strcmp(argv[1], "export-functions") == 0)
    export_functions(file);

  fclose(file);
  return 0;
}