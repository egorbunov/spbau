#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

namespace F {
    struct point_t {
        int x;
        int y;
        friend istream& operator>>(istream& in, point_t &p) {
            return in >> p.x >> p.y;
        }
        friend ostream& operator<<(ostream& out, point_t &p) {
            return out << p.x << " " << p.y;
        }
    };
    typedef point_t edge;

    const int MAX_N = 200;
    point_t points[MAX_N];
    int closest_to[MAX_N]; // closest_to[i] = closest point to i in MST
    bool in_mst[MAX_N];
    int n;



    double sq_dist(point_t& p1, point_t& p2) {
        return (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y);
    }

    double dist(point_t& p1, point_t& p2) {
        return sqrt(sq_dist(p1, p2));
    }

    void solve() {
        cin >> n;
        for (int i = 0; i < n; ++i) {
            cin >> points[i];
        }

        in_mst[0] = true;
        int v_in_mst = 1;
        double sum_edge_len = 0.0;

        edge edges[MAX_N];

        // build mst
        while (v_in_mst < n) {
            int to_add = -1;
            double min_sq_dist = numeric_limits<double>::max();
            for (int i = 0; i < n; ++i) {
                if (in_mst[i]) continue;

                double sq_d = sq_dist(points[i], points[closest_to[i]]);
                if (sq_d < min_sq_dist) {
                    min_sq_dist = sq_d;
                    to_add = i;
                }
            }

            edges[v_in_mst - 1] = { to_add + 1, closest_to[to_add] + 1};

            in_mst[to_add] = true;
            sum_edge_len += sqrt(min_sq_dist);
            v_in_mst += 1;

            // recalculate closest_to

            for (int i = 0; i < n; ++i) {
                if (in_mst[i]) continue;
                double old_dist = sq_dist(points[i], points[closest_to[i]]);
                double new_dist = sq_dist(points[i], points[to_add]);
                if (new_dist < old_dist) {
                    closest_to[i] = to_add;
                }
            }
        }


        // print
        cout.precision(17);
        cout << sum_edge_len << endl;
        cout << v_in_mst - 1 << endl;

        for (int i = 0; i < v_in_mst - 1; ++i) {
            cout << edges[i] << endl;
        }
    }
}

//int main() {
//    F::solve();
//    return 0;
//}