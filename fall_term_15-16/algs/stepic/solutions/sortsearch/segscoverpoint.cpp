#include <algorithm>
#include <iostream>
#include <vector>

namespace {
    using namespace std;

    enum PointType {
        OPEN, CLOSE, TARGET
    };
    struct Point {
        int x;
        int index;
        PointType type;
        Point (int x, PointType type, int index = -1) : x(x), type(type), index(index) {}

        bool operator<(const Point &point) const {
            if (x == point.x) {
                if ((type == OPEN && point.type == CLOSE) ||
                        (type == TARGET && point.type == CLOSE) ||
                        (type == OPEN && point.type == TARGET)) {
                    return true;
                }
            }
            return x < point.x;
        }
    };

    void solve() {
        size_t n, m;
        cin >> n >> m;

        vector<Point> points;
        PointType tmpType = OPEN;
        int tmpX;
        for (size_t i = 0; i < n * 2; ++i) {
            cin >> tmpX;
            points.push_back(Point(tmpX, tmpType));
            tmpType = (tmpType == OPEN) ? CLOSE : OPEN;
        }
        for (size_t i = 0; i < m; ++i) {
            cin >> tmpX;
            points.push_back(Point(tmpX, TARGET, i));
        }

        sort(points.begin(), points.end());

        vector<int> ans(m);
        int openedSegNum = 0;
        for (size_t i = 0; i < points.size(); ++i) {
            switch (points[i].type) {
                case TARGET:
                    ans[points[i].index] = openedSegNum;
                    break;
                case OPEN:
                    openedSegNum += 1;
                    break;
                case CLOSE:
                    openedSegNum -= 1;
                    break;
            }
        }

        for (size_t i = 0; i < ans.size(); ++i) {
            cout << ans[i] << " ";
        }
    }
}

int main() {
    solve();
}
