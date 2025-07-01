#include <bits/stdc++.h>
 
using namespace std;
#define int long long
 
//ifstream fin("input.txt");
//ofstream fout("output.txt");
 
void find_hamiltonian_cycle_Dirak(int n, vector<vector<int>> g) {
    deque<int> q;
    for (int i = 0; i < n; i++) {
        q.push_back(i);
    }
 
    for (int i = 0; i < n * (n - 1); i++) {
        auto it1 = q.begin();
        int v1 = *it1;
        q.erase(it1);
 
        auto it2 = q.begin();
        int v2 = *it2;
 
        if (g[v1][v2] == 0) {
            auto it3 = it2;
            int j = 2;
            while (!(g[v1][*next(it3, j-1)] == 1 && g[v2][*next(it3, j)] == 1)) 
                ++j;
 
            vector<int> subQueue;
            for (int k = 1; k <= j; ++k) 
                subQueue.push_back(*next(it3, k-1));
 
            reverse(subQueue.begin(), subQueue.end());
            for (int k = 1; k <= j; ++k)
                *next(it3, k-1) = subQueue[k-1];
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
    cin.tie(0);
    cout.tie(0);
     
    int n; cin >> n;
    vector<vector<int>> g(n, vector<int>(n));
     
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < i; j++) {
            char c; cin >> c;
            g[i][j] = g[j][i] = (c == '1');
        }
    }
 
    find_hamiltonian_cycle_Dirak(n, g);
    return 0;
 
}