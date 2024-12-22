#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>

#define Num_bits uint16_t
#define Num_spicif "%" SCNu16

void binary_output(Num_bits num, Num_bits kolvo) {
    // функция двоичного вывода
    Num_bits outer = 1; outer <<= (sizeof(Num_bits) * 8 - 1);
    for (Num_bits i = 1; i <= kolvo * 4; ++i) {
        printf("%d", ((num & outer) != 0));
        outer >>= 1;
        if (i % 8 == 0){
            printf(" ");
        }
    }
    printf("\n");
}

Num_bits get_rank(Num_bits nun) {
    // функция получения ранга тетрады
    Num_bits result = 0, chuker = 1;
    for (Num_bits j = 0; j < 4; ++j) {
        result += ((nun & (chuker)) != 0);
        chuker <<= 1;
    }
    switch (result) {
            case 4:
            case 0:
                return 4;
            case 3:
            case 1:
                return 2;
            default:
                return 0;
    }
}

int main(int argc, char *argv[]){
    srand(time(NULL));
    Num_bits Number = rand(), kolvo_tetrad = sizeof(Num_bits) * 2, highest_rank = 0,\
    transfer_mask = 0, left_shift = (kolvo_tetrad * 4) - 4, right_shift = 0, const_tetrada_shift = 4, cart = 0xf, flag = 0;
    
    // проверка ввода пользователя
    if (argc == 2) { 
        if (sscanf(argv[1], Num_spicif, &Number) == 0){
            printf("Ошибка: \'%s\' не является числом\n", argv[1]);
            return 0;
        } else {
            char *vvod = argv[1];
            Num_bits j = 0;
            while (isalnum(vvod[j])){
                if (isdigit(vvod[j]) == 0){ printf("Ошибка: \'%s\' \
                 не является числом\n", argv[1]); return 0;}
                j++;
            }
        }
    }

    // двоичный вывод
    binary_output(Number, kolvo_tetrad);

    // ранг тетрады определим подсчётом единиц. Если единиц 4/0 -> 4, если 3/1 -> 2, если 2 -> 0
    for (Num_bits i = 0; i < kolvo_tetrad; ++i) {
        highest_rank = (highest_rank < (get_rank(Number >> 4 * i))) ? \
        (get_rank(Number >> 4 * i)) : highest_rank;
    }
    
    for (Num_bits j = 0; j < 4; ++j){
        flag += ((get_rank(Number >> 4 * j)) == highest_rank);
    }
    if (flag == kolvo_tetrad) {
        binary_output(Number, kolvo_tetrad);
        return 0;
    }

    //узнав highest_rank, перетасуем тетрады в необходимом порядке
    for (Num_bits i = 0; i < kolvo_tetrad; ++i) {
        cart = 0xf;
        if (get_rank(Number) == highest_rank) {
            cart = Number & cart;
            cart <<= left_shift;
            transfer_mask |= cart;
            Number >>= const_tetrada_shift;
            left_shift -= const_tetrada_shift;
        } else {
            cart = Number & cart;
            cart <<= right_shift;
            transfer_mask |= cart;
            Number >>= const_tetrada_shift;
            right_shift += const_tetrada_shift; 
        }
    }
    Number |= transfer_mask;

    // двоичный вывод
    binary_output(Number, kolvo_tetrad);

    return 0;
}