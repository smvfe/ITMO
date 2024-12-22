#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

//--------------------------------------------//

#define IPv4_length 16

typedef unsigned long long ull;
typedef long long ll;

enum Errors {
    MALLOCERR = 1,
    HOWMANY = 2,
    FILEERR = 3,
    VALIDERR = 4,
};


typedef struct Node {

    char txt[IPv4_length];

    struct Node *previous;
    struct Node *next;
    
} Node;




typedef struct {

    char IP[IPv4_length];
    size_t ind;

} node_pair;

//--------------------------------------------//