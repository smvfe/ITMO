#include <bits/stdc++.h>
  
using namespace std;
#define int long long
  
//ifstream fin("input.txt");
//ofstream fout("output.txt");

bool compT(int a, int b){
    return g[a][b];
}

signed main() {
    ios_base::sync_with_stdio(false);
    std::cin.tie(0);
    std::cout.tie(0);
  
    int n, m; cin >> n;
    g.resize(n, vector<bool>(n));
 
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < i; j++) {
            char c; cin >> c;
            if(c == '1') g[i][j] = true;
            if(c == '0') g[j][i] = true;
        }
    }
 
    vector<int> vert(n);
    vector<int> C;
    iota(vert.begin(), vert.end(), 0);
    stable_sort(vert.begin(), vert.end(), compT);
 
    
    int st = vert[0];
    int pos = 0;
    for (int i = n - 1; i >= 2; --i) {
        if (g[vert[i]][st]) {
            pos = i;
            break;
        }
    }
    C.insert(C.begin(), vert.begin(), vert.begin() + pos + 1);
    vert.erase(vert.begin(), vert.begin() + pos + 1);
 
    for (auto st = vert.begin(); st != vert.end();) {
        auto item = C.begin();
        while (item != C.end() && g[*item][*st]) item++;
        if (item != C.end()) {
            C.insert(item, vert.begin(), st + 1);
            vert.erase(vert.begin(), st + 1);
            st = vert.begin();
        } else {
            st++;
        }
    }
     
    vert = C;
 
    for (int i : vert) 
        cout << i+1 << " ";
 
    return 0;
}