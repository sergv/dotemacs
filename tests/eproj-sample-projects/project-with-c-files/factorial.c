
#include <factorial.h>

bigint factorial(bigint n)
{
        bigint res = 1;
        while (n > 0) {
                res *= n;
        }
        return res;
}

