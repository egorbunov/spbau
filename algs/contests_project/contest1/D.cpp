#include <iostream>

namespace contest1D {

    void print_viki(int end) {
        const char str[] = "viki";
        for (int i = 0; i < end; ++i) {
            std::cout << str[i];
        }
    }

    void solve() {
        using namespace std;
        const size_t BUFFER_SIZE = 2048;
        char buffer[BUFFER_SIZE];

        cin.rdbuf();
        cin.rdbuf()->pubsetbuf(buffer, BUFFER_SIZE);

        char x;
        char buf[4];
        int m = 0;
        while (cin >> x) {
            if (x == '-') {
                cout << "v";
            } else if (x == 'v') {
                if (m != 0) {
                    print_viki(m);
                    m = 0;
                } else
                    m += 1;
            } else if (x == 'i') {
                if (m != 1) {
                    if (m != 3) {
                        print_viki(m);
                    } else
                        cout << "entsiklo";
                    m = 0;
                } else
                    m += 1;
                    
            } else if (x == 'k') {
                if (m != 2) {
                    print_viki(m);
                    m = 0;
                } else
                    m += 1;
            }
        }

    }
}

int main() {
    contest1D::solve();
}
