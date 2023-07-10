#include <stdio.h>
#include <stdbool.h>

int main()
{
    int numero;
    bool esCero;

    printf("Ingrese un número: ");
    scanf("%d", &numero);

    if (numero == 0)
    {
        esCero = true;
    } else 
    {
        esCero = false;
    }

    printf("%s\n", esCero ? "True" : "False");
    return 0;
}
