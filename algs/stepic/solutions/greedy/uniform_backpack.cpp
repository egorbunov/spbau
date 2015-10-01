#include <iostream>
#include <vector>
#include <algorithm>
#include <iomanip>

using namespace std;

namespace {
  struct Thing {
    int w; // weight
    int c; // cost
    double nc; // normalized be weight cost = c / w

    Thing(int w, int c) : w(w), c(c) {
      nc = c / (double) w;
    }

    inline bool operator<(const Thing &thing) const {
      return nc < thing.nc;
    }
  };

  void solve() {
    int n;
    double W;
    cin >> n >> W;
    vector<Thing> things;
    while (n-- > 0) {
      int w, c;
      cin >> c >> w;
      things.push_back(Thing(w, c));
    }
    sort(things.rbegin(), things.rend());
    double opt_cost = 0.0;
    for (int i = 0; i < things.size(); ++i) {
      double frac = (things[i].w <= W ? 1 : W / things[i].w);
      opt_cost += frac * things[i].c;
      W -= frac * things[i].w;
      if (W <= 0) {
        break;
      }
    }
    cout << showpoint << setprecision(15) << std::setfill('0') << opt_cost << endl;
  }
}
