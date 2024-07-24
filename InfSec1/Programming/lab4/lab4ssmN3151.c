#include "libs/libb.h"


//--------------------------------------------//

int compare(const void *a, const void *b);

void mallocCheck(void* ptr);

bool isFileCorrect(char* name);

//--------------------------------------------//

node_pair* readWordsFromFile(FILE* file, ull cnt);

void readIndexesFromFile(FILE* file, ull cnt, ull file_size, node_pair* words);

//--------------------------------------------//

Node* createNode(char *pair);

void push_back(Node **head, Node **tail, char *pair, ull *cnt);

void push_front(Node **head, char *pair, ull *cnt);

void pop_front(Node **head, Node **tail, ull *cnt);

void pop_back(Node **head, Node **tail, ull *cnt);

void swap(Node **head, ll a, ll b, ull cnt);

void dump(Node **head, ull cnt, char* name);

//--------------------------------------------//



int main(int argc, char **argv){

    //~~~~~~~~~~~~~Init~~~~~~~~~~~~~~~~//

    node_pair* words_buffer = NULL;
    Node *head = createNode("");
    Node *tail = head;
    ull *cnt = malloc(sizeof(ull));
    *cnt = 0;
    bool is_empty_list = false;


    //~~~~~~~~~~~~~Valid~~~~~~~~~~~~~~//

    if (argc != 2){

        fprintf(stderr, "Wrong number of arguments\n");
        exit(HOWMANY);

    } else if (strcmp(argv[1], "-v") == 0){

        printf("Mochekov Semyon Sergeevich, N3151\nTask: 2-1-17-1\n");
        return 0;

    }

    //~~~~~~~~~~~~~File~~~~~~~~~~~~~~//

    FILE *source = fopen(argv[1], "r"); 

    if (!source){

        is_empty_list = true; 

    } else {

        head->previous = head->next = NULL;
        fseek(source, 0, SEEK_END);
        ull file_size = ftell(source);
        rewind(source);

        // работа с файлом
        if (file_size != 0){

            char* cnt_word = malloc(32);
            mallocCheck(cnt_word);
            fscanf(source, "%s ", cnt_word);
            *cnt = strtoull(cnt_word, NULL, 10);
            free(cnt_word);

            words_buffer = readWordsFromFile(source, *cnt);
            fseek(source, 0, SEEK_END);
            readIndexesFromFile(source, *cnt, file_size, words_buffer);
            fclose(source);
            qsort(words_buffer, *cnt, sizeof(node_pair), compare);

        } else {

            is_empty_list = true;

        }

    }


    if(!is_empty_list) {

        strcpy(tail->txt, (words_buffer[0].IP));
        for (ull i = 1; i < *cnt; i++) {

            push_back(&head, &tail, (char*)(words_buffer[i].IP), cnt);
            --(*cnt);

        }

    }

    if (words_buffer){
        free(words_buffer);
    }

    //~~~~~~~~~~Online-ans~~~~~~~~~~~//

    int c;
    bool is_changed = false;
    char* line = malloc(256 * sizeof(char));
    while((c = getchar()) != EOF) {

        int i = 0;
        line = realloc(NULL, 256 * sizeof(char));
        mallocCheck(line);
        line[i++] = (char)c;

        while ((c = getchar()) != '\n'){

            line[i++] = (char)c;

        }
        line[i] = '\0';

        char* tok = strtok_r(line, " ", &line);
        while (tok != NULL) {

            if (strcmp(tok, "dump") == 0) {

                tok = strtok_r(NULL, " ", &line);
                dump(&head, *cnt, tok);

            } else if (strcmp(tok, "push_back") == 0) {

                while ((tok = strtok_r(NULL, " ", &line)) != NULL) {

                    char* tok2 = malloc(strlen(tok) + 1);
                    strcpy(tok2, tok);
                    if (isValidIpAddress(tok2)){

                        push_back(&head, &tail, tok, cnt);
                        is_changed = true;

                    } else {

                        fprintf(stderr, "Incorrect IP address\n");
                        goto contin;

                    }

                    free(tok2);

                }
            } else if (strcmp(tok, "push_front") == 0) {

                while ((tok = strtok_r(NULL, " ", &line)) != NULL) {
                    
                    char* tok2 = malloc(strlen(tok) + 1);
                    strcpy(tok2, tok);
                    if (isValidIpAddress(tok2)){

                        push_front(&head, tok, cnt);
                        is_changed = true;

                    } else {

                        fprintf(stderr, "Incorrect IP address\n");
                        goto contin;

                    }

                    free(tok2);

                }
            } else if (strcmp(tok, "pop_back") == 0) { 

                pop_back(&head, &tail, cnt);
                is_changed = true;

            } else if (strcmp(tok, "pop_front") == 0) {

                pop_front(&head, &tail, cnt);
                is_changed = true;

            } else if (strcmp(tok, "swap") == 0) {

                ll k = 0, l = 0;
                char* endpt_k;
                char* endpt_l;

                tok = strtok_r(NULL, " ", &line); 
                if (tok != NULL) {
                    k = strtoll(tok, &endpt_k, 10);
                }

                tok = strtok_r(NULL, " ", &line); 
                if (tok != NULL) {
                    l = strtoll(tok, &endpt_l, 10);
                }

                if (tok == NULL || *endpt_k != '\0' || *endpt_l != '\0' ||
                    k < 0 || l < 0 || (ull)k >= *cnt || (ull)l >= *cnt) {
                    fprintf(stderr, "Uncorrect index\n");
                    goto contin;
                }

                swap(&head, k, l, *cnt);
                is_changed = true;

            } else {

                fprintf(stderr, "Unapropriate command\n");
                goto contin;

            }

            tok = strtok_r(NULL, " ", &line); // Move to the next token

        }

        contin:

    }    

    //~~~~~~~~~~~~~End~~~~~~~~~~~~~//

    if(is_changed) {
        dump(&head, *cnt, "result.txt");
    }
    
    return 0;

}