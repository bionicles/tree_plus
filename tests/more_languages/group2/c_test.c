// c_test.c
#include <stdio.h>

struct Point
{
    int x;
    int y;
};

struct Point getOrigin()
{
    struct Point origin = {0, 0};
    return origin;
}

float mul_two_floats(float x1, float x2)
{
    return x1 * x2; // Use return to return a value
}

enum days
{
    SUN,
    MON,
    TUE,
    WED,
    THU,
    FRI,
    SAT
};

long add_two_longs(long x1, long x2)
{
    return x1 + x2; // Use return to return a value
}

double multiplyByTwo(double num)
{
    return num * 2.0;
}

char getFirstCharacter(char *str)
{
    return str[0];
}

void greet(Person p)
{
    printf("Hello, %s\n", p.name);
}

typedef struct
{
    char name[50];
} Person;


int main()
{
    Person person;
    strcpy(person.name, "World");
    greet(person);
    return 0;
}

int* getArrayStart(int arr[], int size)
{
    return arr; // arr is equivalent to &arr[0]
}