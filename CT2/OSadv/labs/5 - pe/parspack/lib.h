#include "constants.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void input_check(int argc) {
  if (argc != 3) {
    fprintf(stderr, "Arguments must satisfy: <operation> <file path>\n");
    exit(1);
  }
}

void is_pe(FILE *file) {
  int32_t pe_offset;
  char signature[4];

  // find the start pos of PE signature
  fseek(file, PE_SIGNATURE_SHIFT, SEEK_SET);
  fread(&pe_offset, PE_SIGNATURE_SIZE, 1, file);

  // read is-PE signature
  fseek(file, pe_offset, SEEK_SET);
  fread(signature, sizeof(char), PE_SIGNATURE_SIZE, file);

  // check if the signature is correct
  if (signature[0] != 'P' || signature[1] != 'E' || signature[2] != '\0' ||
      signature[3] != '\0') {
    printf("Not PE\n");
    exit(1);
  } else {
    printf("PE\n");
    exit(0);
  }
}

void import_functions(FILE *file) {
  int32_t pe_offset;
  char optional_header[OPTIONAL_HEADER_SIZE];

  // move to Optional Header and read it
  fseek(file, PE_SIGNATURE_SHIFT, SEEK_SET);
  fread(&pe_offset, PE_SIGNATURE_SIZE, 1, file);
  fseek(file, pe_offset + PE_SIGNATURE_SIZE + COFF_SIZE, SEEK_SET);

  // read Optional Header
  fread(optional_header, OPTIONAL_HEADER_SIZE, 1, file);

  // get import table RVA and size
  int32_t import_table_rva =
      *(int32_t *)(optional_header + IMPORT_TABLE_ADDRESS_SHIFT);
  //int32_t import_table_size =
  //  *(int32_t *)(optional_header + IMPORT_TABLE_SIZE_SHIFT);

  long pos = ftell(file), headers_shift = 0;
  // find the section that contains the import table info
  while (1) {
    char section_header[SECTION_HEADER_SIZE];
    fseek(file, pos + SECTION_HEADER_SIZE * headers_shift++, SEEK_SET);
    fread(section_header, 1, SECTION_HEADER_SIZE, file);

    int32_t section_virtual_size =
        *(int32_t *)(section_header + SECTION_VIRTUAL_SIZE_SHIFT);
    int32_t section_rva = *(int32_t *)(section_header + SECTION_RVA_SHIFT);
    int32_t section_raw = *(int32_t *)(section_header + SECTION_RAW_SHIFT);

    if (import_table_rva >= section_rva &&
        import_table_rva < section_rva + section_virtual_size) {

      int32_t import_raw = section_raw + import_table_rva - section_rva;

      long imp_depd_shift = 0;
      while(1) {
        char name[256];
        char dependency[DEPENDENCY_SIZE];

        // read 20bt depd
        fseek(file, import_raw + imp_depd_shift++ * DEPENDENCY_SIZE, SEEK_SET);
        fread(dependency, DEPENDENCY_SIZE, 1, file);

        // is depd null
        char is_empty = 1;
        for (int j = 0; j < DEPENDENCY_SIZE; j++) {
          if (dependency[j] != 0) {
            is_empty = 0;
            break;
          }
        }
        if (is_empty == 1)
          break;

        int32_t lookup_table_rva = *(int32_t *)(dependency);
        int32_t name_rva = *(int32_t *)(dependency + RVA_LIB_NAME_SHIFT);

        int32_t lookup_table_raw = section_raw + lookup_table_rva - section_rva;

        fseek(file, section_raw + name_rva - section_rva, SEEK_SET);
        fgets(name, sizeof(name), file);
        printf("%s\n", name);

        // get lib's functions
        long func_shift = 0;
        while (1) {
          char func_name[256];
          int64_t lookup_entry;

          fseek(file, lookup_table_raw + 8 * func_shift++, SEEK_SET);
          fread(&lookup_entry, 8, 1, file);

          if (lookup_entry == 0)
            break;
          if (lookup_entry & LEADING_BIT)
            continue;

          int32_t func_name_rva = (int32_t)(lookup_entry & FOLLOWING_31_BITS);
          int32_t func_name_raw =
              section_raw + func_name_rva - section_rva + 2; // skip the hint

          fseek(file, func_name_raw, SEEK_SET);
          fgets(func_name, sizeof(func_name), file);
          printf("    %s\n", func_name);
        }
      }

      break;
    }
  }
}

void export_functions(FILE *file) {
  int32_t pe_offset;
  char optional_header[OPTIONAL_HEADER_SIZE];

  // read PE-header
  fseek(file, PE_SIGNATURE_SHIFT, SEEK_SET);
  fread(&pe_offset, PE_SIGNATURE_SIZE, 1, file);
  fseek(file, pe_offset + PE_SIGNATURE_SIZE + COFF_SIZE, SEEK_SET);

  // read Optional Header
  fread(optional_header, OPTIONAL_HEADER_SIZE, 1, file);

  // get RVA of Export Table
  int32_t export_table_rva =
      *(int32_t *)(optional_header + EXPORT_TABLE_ADDRESS_SHIFT);

  // find the section that contains the export table info
  while (1) {
    char section_header[SECTION_HEADER_SIZE];
    fread(section_header, SECTION_HEADER_SIZE, 1, file);

    int32_t section_virtual_size =
        *(int32_t *)(section_header + SECTION_VIRTUAL_SIZE_SHIFT);
    int32_t section_rva = *(int32_t *)(section_header + SECTION_RVA_SHIFT);
    int32_t section_raw = *(int32_t *)(section_header + SECTION_RAW_SHIFT);

    if (export_table_rva >= section_rva &&
        export_table_rva < section_rva + section_virtual_size) {
      int32_t export_raw = section_raw + export_table_rva - section_rva;

      int32_t func_addresses_rva, name_pointers_rva, ordinals_rva;
      fseek(file, export_raw, SEEK_SET);

      // export table headers
      int32_t name_rva, ordinal_base, num_funcs, num_names;
      fseek(file, export_raw + EXPORT_CHARACTERISTICS_SHIFT,
            SEEK_SET); // skip characteristics
      fread(&name_rva, FIELD_SIZE_4, 1, file);
      fread(&ordinal_base, FIELD_SIZE_4, 1, file); // base follow num
      fread(&num_funcs, FIELD_SIZE_4, 1, file);
      fread(&num_names, FIELD_SIZE_4, 1, file);

      // read pointers for names and address
      fread(&func_addresses_rva, FIELD_SIZE_4, 1, file);
      fread(&name_pointers_rva, FIELD_SIZE_4, 1, file);
      fread(&ordinals_rva, FIELD_SIZE_4, 1, file); // numbers for named funcs

      // address of arrays
      func_addresses_rva = section_raw + func_addresses_rva - section_rva;
      name_pointers_rva = section_raw + name_pointers_rva - section_rva;
      ordinals_rva = section_raw + ordinals_rva - section_rva;

      for (int32_t i = 0; i < num_names; i++) {
        int32_t func_name_raw;
        fseek(file, name_pointers_rva + i * 4, SEEK_SET);
        fread(&func_name_raw, 4, 1, file);

        func_name_raw = section_raw + func_name_raw - section_rva;
        fseek(file, func_name_raw, SEEK_SET);

        char func_name[256];
        fgets(func_name, sizeof(func_name), file);
        printf("%s\n", func_name);
      }
      break;
    }
  }
}
