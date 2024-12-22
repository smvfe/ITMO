#include <bits/stdc++.h>
  
using namespace std;
#define int long long
  
//ifstream fin("input.txt");
//ofstream fout("output.txt");

bool compLMPS(int i, int j) {
    cout << 1 << " " << i << " " << j << endl;
    string ans;
    cin >> ans;
    return (ans[0] == 'Y');
}

signed main() {
    ios_base::sync_with_stdio(false);
    std::cin.tie(0);
    std::cout.tie(0);
       
    int n, m; cin >> n;
  
    deque<int> lamps;
    for (int i = 0; i < n; ++i)
        lamps.push_back(i + 1);
       
    stable_sort(lamps.begin(), lamps.end(), compLMPS); // crucial stable sorting
   
    cout << 0 << " ";
    while (!lamps.empty()) {
        cout << lamps.front() << " ";
        lamps.pop_front();
    }
}