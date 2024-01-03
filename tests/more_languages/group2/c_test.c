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

long complexFunctionWithMultipleArguments(
    int param1,
    double param2,
    char *param3,
    struct Point point
) {
    // Some complex logic here
    long result = param1 + (long)param2 + param3[0] + point.x + point.y;
    return result;
}

// edge case examples from redis acl
/* Create a new key pattern. */
keyPattern *ACLKeyPatternCreate(sds pattern, int flags) {
    keyPattern *new = (keyPattern *) zmalloc(sizeof(keyPattern));
    new->pattern = pattern;
    new->flags = flags;
    return new;
}


/* Append the string representation of a key pattern onto the
 * provided base string. */
sds sdsCatPatternString(sds base, keyPattern *pat) {
    if (pat->flags == ACL_ALL_PERMISSION) {
        base = sdscatlen(base,"~",1);
    } else if (pat->flags == ACL_READ_PERMISSION) {
        base = sdscatlen(base,"%R~",3);
    } else if (pat->flags == ACL_WRITE_PERMISSION) {
        base = sdscatlen(base,"%W~",3);
    } else {
        serverPanic("Invalid key pattern flag detected");
    }
    return sdscatsds(base, pat->pattern);
}

/* Checks a channel against a provided list of channels. The is_pattern 
 * argument should only be used when subscribing (not when publishing)
 * and controls whether the input channel is evaluated as a channel pattern
 * (like in PSUBSCRIBE) or a plain channel name (like in SUBSCRIBE). 
 * 
 * Note that a plain channel name like in PUBLISH or SUBSCRIBE can be
 * matched against ACL channel patterns, but the pattern provided in PSUBSCRIBE
 * can only be matched as a literal against an ACL pattern (using plain string compare). */
static int ACLCheckChannelAgainstList(list *reference, const char *channel, int channellen, int is_pattern) {
    listIter li;
    listNode *ln;

    listRewind(reference, &li);
    while((ln = listNext(&li))) {
        sds pattern = listNodeValue(ln);
        size_t plen = sdslen(pattern);
        /* Channel patterns are matched literally against the channels in
         * the list. Regular channels perform pattern matching. */
        if ((is_pattern && !strcmp(pattern,channel)) || 
            (!is_pattern && stringmatchlen(pattern,plen,channel,channellen,0)))
        {
            return ACL_OK;
        }
    }
    return ACL_DENIED_CHANNEL;
}

// https://github.com/redis/redis/blob/9d0158bf89265daa96e1711478102147117f6b14/src/redis-benchmark.c#L81
static struct config {
    aeEventLoop *el;
    cliConnInfo conn_info;
    const char *hostsocket;
    int tls;
    struct cliSSLconfig sslconfig;
} config;

