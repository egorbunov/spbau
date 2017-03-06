//
// Created by egorbunov on 26.10.15.
//

#include <iostream>
#include <algorithm>
#include <stack>

/**
 * Task H
 */
namespace contest1H {

    struct Point {
        bool isOpen;
        int x;
        int id;

        Point() {}
        Point(bool isOpen, int x, int id) : isOpen(isOpen), x(x), id(id) {}

        bool operator<(const Point &p) const {
            if (!p.isOpen && x == p.x)
                return false;
            if (!isOpen && x == p.x)
                return true;
            return (x < p.x);
        }

        friend std::ostream& operator<<(std::ostream& out, const Point& p) {
            out << "{ " << (p.isOpen ? "open" : "close") << ", " << p.x << " }" << p.id;
        }
    };

    void solve() {

        int n;
        std::cin >> n;
        std::vector<Point> pts(2 * n);
        bool isOpen = true;
        for (int i = 0, x; i < pts.size(); ++i) {
            std::cin >> x;
            pts[i] = Point(isOpen, x, i / 2);
            isOpen = !isOpen;
        }

        std::sort(pts.begin(), pts.end());

//        for (int i = 0; i < pts.size(); ++i) {
//            std::cout << pts[i] << std::endl;
//        }

        std::vector<int> ans(n);
        std::stack<int> freeRooms;
        int cnt = 0;
        int maxCnt = 0;
        int maxRoom = 1;
        for (int i = 0; i < pts.size(); ++i) {
            if (pts[i].isOpen) {
                cnt += 1;
                if (freeRooms.empty())
                    ans[pts[i].id] = maxRoom++;
                else {
                    ans[pts[i].id] = freeRooms.top();
                    freeRooms.pop();
                }
            } else {
                cnt -= 1;
                freeRooms.push(ans[pts[i].id]);
            }
            if (cnt > maxCnt)
                maxCnt = cnt;
        }

        std::cout << maxCnt << std::endl;
        for (int i = 0; i < ans.size(); ++i) {
            std::cout << ans[i] << " ";
        }

    }
}