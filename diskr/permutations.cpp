#include <iostream>

template<typename T> 
void print(int n, T* arr) {
	for (int i = 0; i < n; ++i) {
		std::cout << arr[i] << " ";
	}
	std::cout << std::endl;
}

void gen(int* perm, bool* used, int n, int k, int ind) {
	for (int i = 0; i < n; ++i) {
		if (!used[i]) {
			perm[ind] = i;
			used[i] = true;
			if (ind + 1 < k) {
				gen(perm, used, n, k, ind + 1);
			} else {
				print(k, perm);
			}
			used[i] = false;
		}
	}
}

int main() {
	using namespace std;
	int n = 0, k = 0;
	cin >> n >> k;
	bool* used = new bool[n];
	int* perm = new int[k];
	gen(perm, used, n, k, 0);
	return 0;
}