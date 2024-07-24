#include <stdio.h>

int main(){
    int Number = 0xF15AD788;
    int *Index = &Number; // Получаем адрес числа в памяти компьютера
    // Выводим каждый байт из памяти компьютера
    for (int i = 0; i<4; i++) {
        unsigned char Bytte = *((char*)Index+i); 
        printf("| %x |", Bytte);
    }
    printf("\n");
    return 0;
}