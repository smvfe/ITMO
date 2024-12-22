#include <bits/stdc++.h>
  
using namespace std;
#define int long long
  
//ifstream fin("input.txt");
//ofstream fout("output.txt");

void bfs_clr(int start, vector<vector<int>>& g, vector<int>& clr, int k) {
    queue<int> q;
    q.push(start);
     
    while (!q.empty()) {
        int v = q.front();
        q.pop();
         
        vector<bool> used_colors(k, false);
        for (int u : g[v]) {
            if (clr[u] != -1) {
                used_colors[clr[u]] = true;
            }
        }
         
        for (int color = 0; color < k; ++color) {
            if (!used_colors[color]) {
                clr[v] = color;
                break;
            }
        }
         
        for (int u : g[v]) {
            if (clr[u] == -1) {
                q.push(u);
            }
        }
    }
}

signed main() {
    ios_base::sync_with_stdio(false);
    std::cin.tie(0);
    std::cout.tie(0);
  
    int n, m; cin >> n >> m;
    vector<vector<int>> g(n);
    vector<int> deg(n, 0);
    
    for (int i = 0; i < m; ++i) {
        int a, b;
        cin >> a >> b;
        --a; --b; 
        g[a].push_back(b);
        g[b].push_back(a);
        ++deg[a]; ++deg[b];
    }
    int max_deg = *max_element(deg.begin(), deg.end());
    int k = max_deg + (max_deg % 2 == 0 ? 1 : 0);
  
    vector<int> vertices(n);
    iota(vertices.begin(), vertices.end(), 0);
    sort(vertices.begin(), vertices.end(), [&](int a, int b) {
        if (deg[a] == deg[b]) return g[a].size() > g[b].size();
        return deg[a] > deg[b];
    });
     
    vector<int> clr(n, -1);
    bfs_clr(vertices[0], g, clr, k);
     
    cout << k << endl;
    for (int i = 0; i < n; ++i) {
        cout << clr[i] + 1 << endl;
    }
     
    return 0;
}