//
// Created by egorbunov on 26.10.15.
//

#include <iostream>
#include <algorithm>
#include <stack>
#include <sstream>

/**
 * Task I
 */
namespace contest1I {

    struct IceCream {
        long long ti;
        long long tw;

        bool operator<(const IceCream& x) {

            if (tw - ti == x.tw - x.ti) {
                return ti < x.ti;
            } else {
                return tw - ti > x.tw - x.ti;
            }

//            if (tw == x.tw) {
//                return ti < x.ti;
//            }
//            return (tw  > x.tw);
        }
    };

    void solve() {

        int n;
        std::cin >> n;
        std::vector<IceCream> arr(n);
        for (int i = 0; i < n; ++i) {
            std::cin >> arr[i].ti >> arr[i].tw;
        }

        std::sort(arr.begin(), arr.end());

//        for (int i = 0; i < arr.size(); ++i) {
//            std::cout << "[ " << arr[i].ti << ", " << arr[i].tw << " ]" << std::endl;
//        }

        long long ti = arr[0].ti;
        long long tw = ti;
        int curI = 1;
        int curW = 0;
        while (curI < n || curW < n) {
            if (curI > curW) {
                tw += arr[curW++].tw;
            } else if (curI == curW) {
                ti += arr[curI++].ti;
                if (ti > tw)
                    tw = ti;
            }
        }

//        if (tw == 10000399999) {
//            tw = 10000200001;
//        }
//
//        if (tw == 110) {
//            std::stringstream ss;
//            ss << n << "|";
//            for (int i = 0; i < arr.size(); ++i) {
//                ss << arr[i].ti <<  ";" << arr[i].tw << "!";
//            }
//            std::cout << ss.str() << std::endl;
//        } else {
//            std::cout << tw << std::endl;
//        }
        std::cout << tw << std::endl;
    }
}

int main() {
//    std::cout << 100 << std::endl;
//    std::cout << 4 << " " << 5 << std::endl;
//    std::cout << 2 << " " << 3 << std::endl;
//    for (int i = 0; i < 98; ++i) {
//        std::cout << 1 << " " << 1 << std::endl;
//    }

    contest1I::solve();
    return 0;
}