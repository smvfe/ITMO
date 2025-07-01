#include <bits/stdc++.h>
  
using namespace std;
#define int long long
  
//ifstream fin("input.txt");
//ofstream fout("output.txt");

void build_tree(vector<int> pruf, vector<int> &vert, int n){
    vector<pair<int, int>> tree;
    set<int> not_in_prudf;
    for(int i = 0; i < n; i++)
        if (vert[i] == 0)
            not_in_prudf.insert(i);
 
    for(int i = 0; i < n - 2; i++){
        int u = pruf[i];
        int v = *not_in_prudf.begin();
        not_in_prudf.erase(not_in_prudf.begin());
 
        vert[u]--;
        if (vert[u] == 0)
            not_in_prudf.insert(u);
        tree.push_back({v, u});
    }
    int u = *not_in_prudf.begin();
    not_in_prudf.erase(not_in_prudf.begin());
    int v = *not_in_prudf.begin();
    tree.push_back({u, v});
 
    for(auto [u, v] : tree)
        cout << u+1 << " " << v+1 << "\n";
}

signed main() {
    ios_base::sync_with_stdio(false);
    std::cin.tie(0);
    std::cout.tie(0);
  
    int n; cin >> n;
    vector<int> v(n);
    vector<int> pruf(n-2);
      
    for (int i = 0; i < n-2; i++) {
        int x; cin >> x;
        pruf[i] = --x; 
        v[x]++;
    }
     
    build_tree(pruf, v, n);
    return 0;
  
}