#include <iostream>

const int MOD = 1000000007;

long long fast_pow(long long a, int n)
{
    long long result = 1;
    long long power = n;
    long long value = a;
    while(power>0)
    {
        if (power & 1) {
            result = (result * value) % MOD;
        }
        value = (value * value) % MOD;
        power >>= 1;
    }
    return result;
}

int main() {
    long long res = 1;

    int n;
    std::cin >> n;

    for (int i = 1; i <= n; ++i) {
        res = ((((2 * (2 * i - 1) % MOD) * res) % MOD) * fast_pow(i + 1, MOD-2)) % MOD;
    }

    std::cout << res;
    return 0;
}

