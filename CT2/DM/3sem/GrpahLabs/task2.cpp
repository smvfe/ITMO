#include <bits/stdc++.h>
  
using namespace std;
#define int long long
  
//ifstream fin("input.txt");
//ofstream fout("output.txt");

void find_hamiltonian_cycle_Hvatal(int n, vector<vector<int>> &g) {
    deque<int> q;
    for (int i = 0; i < n; i++) {
        q.push_back(i);
    }
  
    for (int i = 0; i < n * (n - 1); i++) {
        auto it1 = q.begin();
        int v1 = *it1;
        q.pop_front();
  
        auto it2 = q.begin();
        int v2 = *it2;
  
        if (g[v1][v2] == 0) {
            auto it3 = next(it2);
            while (it3 != q.end() && !(g[v1][*prev(it3)] == 1 && g[v2][*it3] == 1)) {
                ++it3;
            }
 
            if (it3 == q.end()) {
                it3 = next(it2);
                while (it3 != q.end() && !g[v1][*it3]) {
                    ++it3;
                }
            }
 
            vector<int> subQueue(it2, it3);
            reverse(subQueue.begin(), subQueue.end());
            copy(subQueue.begin(), subQueue.end(), it2);
        }
  
        q.push_back(v1);
    }
  
    while (!q.empty()) {
        cout << q.front() + 1 << " ";
        q.pop_front();
    }
}

signed main() {
    ios_base::sync_with_stdio(false);
    std::cin.tie(0);
    std::cout.tie(0);
  
    int n, m; cin >> n;
    vector<vector<int>> g(n, vector<int>(n));
     
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < i; j++) {
            char c; cin >> c;
            g[i][j] = g[j][i] = (c == '1');
        }
    }
   
    find_hamiltonian_cycle_Hvatal(n, g);
    return 0;
}