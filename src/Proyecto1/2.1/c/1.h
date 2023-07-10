#include <stdio.h>

int main()
{
    int numero;
    printf("Ingrese el número");
    scanf("%d", &numero);
    if(numero == 0)
    {
        printf("True");
    } else {
        printf("False");
    }
    return 0;
}
