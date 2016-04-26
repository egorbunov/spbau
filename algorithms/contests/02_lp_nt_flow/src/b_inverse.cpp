#include <iostream>
#include <tuple>

using namespace std;

using ll_t = long long;

tuple<ll_t, ll_t, ll_t> ext_gcd(ll_t a, ll_t b) {
    if (a == 0) {
        return make_tuple(b, 0, 1);
    }
    auto t = ext_gcd(b % a, a);
    return make_tuple(get<0>(t), get<2>(t) - (b / a) * get<1>(t), get<1>(t));
};


int main() {
    ll_t a, m;
    cin >> a >> m;

    auto t = ext_gcd(a, m);
    if (get<0>(t) != 1) {
        cout << -1 << endl;
    } else {
        cout << (get<1>(t) % m + m) % m << endl;
    }
}