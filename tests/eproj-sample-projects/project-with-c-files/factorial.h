
#ifndef FACTORIAL_H_
#define FACTORIAL_H_

typedef long long bigint;

struct bigint_list_s {
    bigint item;
    struct bigint_list_s * next;
};

typedef struct bigint_list_s bigint_list_t;

bigint factorial(bigint n);

#endif /* #ifndef FACTORIAL_H_ */

