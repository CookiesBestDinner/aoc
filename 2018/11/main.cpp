#include <iostream>
#include <tuple>

using namespace std;

int pcell(int y, int x) {
    int rackid = (1+x) + 10;
    int powerLevel = rackid * (1+y);
    powerLevel += 7165;
    powerLevel *= rackid;
    powerLevel = (powerLevel / 100) % 10;
    powerLevel -= 5;
    return powerLevel;
}


int subs(int y, int x, int s) {
    int total = 0;
    for (int dy = y; dy < y+s; ++dy) {
        if (dy >= 300) {
            return -9999999;
        }
        for (int dx = x; dx < x+s; ++dx) {
            if (dx >= 300) {
                return -9999999;
            }
            total += pcell(dy, dx);
        }
    }
    return total;
}


tuple<int, int> getScore(int y, int x) {
    bool found = false;
    int best = 0;
    int bestSize = 0;
    for (int size = 10; size <= 18; ++size) {
        auto res = subs(y, x, size);
        if (!found || res > best) {
            bestSize = size;
            best = res;
            found = true;
        }
    }
    return make_tuple(best, bestSize);
}


int main() {
    int best = -99999;
    int size = -1;
    int by = -1;
    int bx = -1;
    for (int y = 0; y < 300; ++y) {
        if (y % 3 == 0) {
            cout << y << endl;
        }
        for (int x = 0; x < 300; ++x) {
            auto res = getScore(y, x);
            auto[mybest, mysize] = res;
            if (mybest > best) {
                best = mybest;
                size = mysize;
                by = y;
                bx = x;
            }
        }
    }
    cout << bx+1 << ", " << by+1 << ", " << size << " (" << best << ")\n";
}
