#include <stdio.h>

// int main()
// {
//     printf("%f\n", 1.1 * 2 + 4 / 3 - 0.2);
//     return 0;
// }

typedef struct
{
    int i;
    double d;
    char c;
    node *next;
} node;

node *getNode(int val, double d, char c)
{
    node a = {val, d, c, NULL};
    return &a;
}

node **getArrOfLL(node *start)
{
    int howMany = 0;
    node *a = start;
    while (a != NULL)
    {
        howMany++;
        a = a->next;
    }
    node **arr = malloc(howMany * sizeof(node *));
    for (int i = 0; i < howMany; i++)
    {
        arr[i] = (start + i);
    }
    return arr;
}
