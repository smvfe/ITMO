#include <bits/stdc++.h>
  
using namespace std;
#define int long long
  
//ifstream fin("input.txt");
//ofstream fout("output.txt");
  
void prufer_code(int n, vector<set<int>>& g, vector<int> degree) {
    vector<int> code;
    set<int> isLeaf;
  
    for(int i = 0; i < n; i++){
        if (degree[i] == 1)
            isLeaf.insert(i);
    }
    for (int i = 0; i < n - 2; i++) {
        int v = *isLeaf.begin();
        int parent = *g[v].begin();
        code.push_back(parent); 
        g[parent].erase(v);
        degree[parent]--;
        degree[v]--;
        isLeaf.erase(v);
        if (degree[parent] == 1)
            isLeaf.insert(parent);
    }
  
    for (int i : code)
        cout << i + 1 << " ";
  
}
  
  
signed main() {
    ios_base::sync_with_stdio(false);
    std::cin.tie(0);
    std::cout.tie(0);
  
    int n; cin >> n;
    vector<set<int>> g(n);
    vector<int> degree(n);
      
    for (int i = 0; i < n-1; i++) {
        int x, y; cin >> x >> y;
        g[x-1].insert(y-1);
        g[y-1].insert(x-1);
        degree[x-1]++;
        degree[y-1]++;
    }
      
    if (n > 2)
        prufer_code(n, g, degree);
    else 
        cout << "";
  
    return 0;
  
}