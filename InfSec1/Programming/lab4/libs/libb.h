#include "types.h"


//-------------------------Блок прочих функций-----------------------------//

int compare(const void *a, const void *b){ 

    return ((node_pair*)a)->ind - ((node_pair*)b)->ind; 

}

void mallocCheck(void* ptr) {

    if (ptr == NULL) {

        fprintf(stderr, "Memory allocation error\n");
        exit(MALLOCERR);

    }

}

bool isValidIpAddress(char *ipAddress) {

    int num, dots = 0;
    char *ptr;

    if (strlen(ipAddress) > 15) {
        return false;
    }

    if (ipAddress == NULL) {
        return false;
    }

    ptr = strtok(ipAddress, ".");

    if (ptr == NULL) {
        return false;
    }

    while (ptr) {

        if (!isdigit(*ptr)) {
            return false;
        }

        num = strtoll(ptr, NULL, 10);

        if (num >= 0 && num <= 255) {

            ptr = strtok(NULL, ".");
            if (ptr != NULL) {

                dots++;

            }

        } else {
            return false;
        }

    }

    if (dots != 3) {
        return false;
    }

    return true;

}

//----------------Блок функций для работы с файлом-------------------------//

node_pair* readWordsFromFile(FILE* file, ull cnt) { 

    node_pair* words = malloc(cnt * sizeof(node_pair));
    char* word = malloc(IPv4_length);

    mallocCheck(words);
    mallocCheck(word);

    int reader;
    ull word_ind_reader = 0;

    for (ull i = 0; i < cnt; i++) {

        word_ind_reader = 0;

        while((reader = fgetc(file)) != EOF && reader != '\0') {

            words[i].IP[word_ind_reader++] = (char)reader;

        }

        words[i].IP[word_ind_reader] = '\0';

    }

    free(word);

    return words;

}

void readIndexesFromFile(FILE* file, ull cnt, ull file_size, node_pair* words) {

    char* buffer = malloc(file_size);
    mallocCheck(buffer);

    fseek(file, 0, SEEK_END);
    char symb = (char)fgetc(file);
    ull i = 0, current_pos;

    while(symb != '\n' && i < file_size) {

        current_pos = ftell(file);
        symb = (char)fgetc(file);
        buffer[i++] = symb;
        fseek(file, current_pos - 1, SEEK_SET);

    }

    char hold;
    for (ull j = 0; j < i/2; j++) {

        hold = buffer[j];
        buffer[j] = buffer[i - j - 1];
        buffer[i - j - 1] = hold;

    }

    char *ind = malloc(32), *str_ptr;
    mallocCheck(ind);
    ind = strtok_r(buffer, " ", &str_ptr);

    for (ull i = 0; i < cnt; i++) {

        words[i].ind = strtoull(ind, NULL, 10);
        ind = strtok_r(NULL, " ", &str_ptr);

    }

    free(buffer);

}

bool isFileCorrect(char* name) {

    if (!strstr(name , ".txt")){

        fprintf(stderr, "Ucorrect file format\n");
        return false;

    }

    return true;

}

//----------------Блок функций для работы с списком-----------------------//

Node* createNode(char *pair) {

    Node *newNode = (Node *)malloc(sizeof(Node));
    mallocCheck(newNode);

    strcpy(newNode->txt, pair);
    newNode->previous = NULL;
    newNode->next = NULL;

    return newNode;

}

void push_front(Node **head, char *pair, ull *cnt) {

    Node* new_node = createNode(pair);
    new_node->next = *head; 

    if (*head != NULL) {
        (*head)->previous = new_node;
    }

    *head = new_node;
    new_node->previous = NULL; 

    (*cnt)++; 
    
}

void push_back(Node **head, Node **tail, char *pair, ull *cnt) {

    Node* new_node = createNode(pair);

    if (*cnt == 0) {

        *head = new_node;
        *tail = new_node;
        new_node->previous = NULL;
        new_node->next = NULL;

    } else {

        new_node->previous = *tail;
        (*tail)->next = new_node;
        *tail = new_node;
        new_node->next = NULL;

    }

    (*cnt)++;
}

void pop_front(Node **head, Node **tail, ull *cnt) {

    if (*head == NULL || *cnt == 0) {
        return;
    }

    Node *to_dump = *head;
    
    if (*cnt > 1) {

        *head = to_dump->next;
        (*head)->previous = NULL;

    } else {
        
        *head = NULL;
        *tail = NULL;
    }
    
    free(to_dump);
    (*cnt)--;

    if (*cnt == 0) {
        
        *head = NULL;
        *tail = NULL;

    }

}

void pop_back(Node **head, Node **tail, ull *cnt) {

    if (*cnt == 0 || *tail == NULL) {
        return;
    }

    Node *to_dump = *tail;
    
    if (*cnt > 1) {

        *tail = to_dump->previous;
        (*tail)->next = NULL;

    } else {

        *head = NULL;
        *tail = NULL;

    }
    
    free(to_dump);
    (*cnt)--;

    if (*cnt == 0) {
        *head = NULL;
        *tail = NULL;
    }

}

void swap(Node **head, ll a, ll b, ull cnt){

    ll t = a < b ? a : b;
    Node *tmp = NULL;
    Node *searcher = (*head);
    char* holder = malloc(IPv4_length);

    for(ull i = 0; i < cnt; i++){

        if((ll)i == t){

            strcpy(holder, searcher->txt);
            tmp = searcher;

        } else if ((ll)i == (t == a ? b : a)) {

            strcpy(tmp->txt, searcher->txt);
            strcpy(searcher->txt, holder);
            break;

        }

        searcher = searcher->next;

    }

}

void dump(Node **head, ull cnt, char* name){

    if (name && isFileCorrect(name)){

        FILE *writer = fopen(name, "w+");

        if (writer){    

            if (cnt > 0){

                Node* searcher = (*head);
                for (ull i = 0; i < cnt; i++){

                    fprintf(writer, "%16p %16p %16p %s\n", (void*)searcher, (void*)(searcher->previous), (void*)(searcher->next), searcher->txt);
                    searcher = searcher->next;

                }

            }

            fclose(writer);

        } else{

            fprintf(stderr, "Can't open new file\n");
            goto contin;

        }
    } else if (name == NULL){

        if (cnt > 0){

            Node* searcher = (*head);
            for (ull i = 0; i < cnt; i++){

                fprintf(stdout, "%16p %16p %16p %s\n", (void*)searcher, (void*)(searcher->previous), (void*)(searcher->next), searcher->txt);
                searcher = searcher->next;

            }

        }

    }

    contin:
}

//---------------------------------------------------------------------//