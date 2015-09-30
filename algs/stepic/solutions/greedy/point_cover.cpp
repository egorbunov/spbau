#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

namespace {
  struct Segment {
    int l, r;

    Segment(int l, int r) : l(l), r(r) { }

    inline bool operator<(const Segment &seg) const {
      return r < seg.r;
    }
  };

/**
 * Cover given segments with minimal number of points, so every segment
 * contain at least one point
 */
  void solve() {
    int n;
    vector<Segment> segments;

    cin >> n;
    while (n-- > 0) {
      int l, r;
      cin >> l >> r;
      segments.push_back(Segment(l, r));
    }

    sort(segments.begin(), segments.end());

    vector<int> pnts(1, -1);
    for (int i = 0; i < segments.size(); ++i) {
      if (segments[i].l > pnts[pnts.size() - 1])
        pnts.push_back(segments[i].r);
    }

    cout << pnts.size() - 1 << endl;
    for (int i = 1; i < pnts.size(); ++i) {
      cout << pnts[i] << " ";
    }
  }
}