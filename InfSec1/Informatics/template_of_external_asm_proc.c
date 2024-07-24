#include <stdio.h>

extern double _stdcall linear(float, float);
// ax + b = 0

int main() {
	float a, b;
	printf("To solve linear equation ax + b = 0...");
	getchar();
	printf("\nEnter a: ");
	scanf_s("%f", &a);
	printf("\nEnter b: ");
	scanf_s("%f", &b);
	printf("\nAnswer: %llf\n", linear(a, b));
	return 0;
}