// $ g++ -O2 -std=c++17 main.cpp && time ./a.out < zdata
// part1: 111935
// #650 has no overlaps.
//
// real    0m0.162s
// user    0m0.151s
// sys     0m0.010s

#include <iostream>
#include <unordered_map>
#include <tuple>
#include <vector>

using namespace std;


int main() {
    char discardCh;
    int n, col, row, width, height;


    unordered_map<int, int> board;
    vector<tuple<int, int, int, int, int> > claims;

    while (cin >> discardCh) {  // #
        cin >> n;
        cin >> discardCh;  // @
        cin >> col;
        cin >> discardCh;  // ,
        cin >> row;
        cin >> discardCh;  // :
        cin >> width;
        cin >> discardCh;  // x
        cin >> height;

        claims.emplace_back(n, col, row, width, height);

        for (int x = col; x < col+width; ++x) {
            for (int y = row; y < row+height; ++y) {
                // auto key = make_tuple(x, y);
                auto key = x * 10000 + y;
                auto it = board.find(key);
                int count = 1;
                if (it != board.end()) {
                    count += it->second;
                }
                board[key] = count;
            }
        }
    }
    // part1: how many locations is covered by >1 claim
    auto part1Ans = 0;
    for (auto kvp : board) {
        if (kvp.second > 1) {
            ++part1Ans;
        }
    }
    cout << "part1: " << part1Ans << endl;

    // part2: what claim is entirely without overlap
    for (auto kvp : claims) {
        auto[n, col, row, width, height] = kvp;
        for (auto x = col; x < col+width; ++x) {
            for (auto y = row; y < row+height; ++y) {
                // auto key = make_tuple(x, y);
                auto key = x * 10000 + y;
                if (board[key] > 1) {
                    goto overlapfound;
                }
            }
        }
        cout << "#" << n << " has no overlaps.\n";
overlapfound: {}
    }
}
