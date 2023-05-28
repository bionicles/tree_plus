// c_test.c
#include <stdio.h>

typedef struct
{
    char name[50];
} Person;

void greet(Person p)
{
    printf("Hello, %s\n", p.name);
}

int main()
{
    Person person;
    strcpy(person.name, "World");
    greet(person);
    return 0;
}
