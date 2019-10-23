#include <vector>
#include <iostream>
#include <map>
using namespace std;

void show(vector<int>& reg) {
    for (auto v : reg) {
        cout << v << " ";
    }
    cout << "\n";
}

inline void op(int opcode, vector<int>& reg, int a, int b, int c) {
    // cout << "recieved opcode: " << opcode << " with args: " << a << " " << b << " " << c << "\n";
    switch (opcode) {
    case 0:
        reg[c] = reg[a] + b;
        break;
    case 1:
        reg[c] = reg[a] + reg[b];
        break;
    case 2:
        reg[c] = reg[a] * b;
        break;
    case 3:
        reg[c] = reg[a] * reg[b];
        break;
    case 4:
        reg[c] = reg[a] & b;
        break;
    case 5:
        reg[c] = reg[a] & reg[b];
        break;
    case 6:
        reg[c] = reg[a] | b;
        break;
    case 7:
        reg[c] = reg[a] | reg[b];
        break;
    case 8:
        reg[c] = a;
        break;
    case 9:
        reg[c] = reg[a];
        break;
    case 10:
        if (a > reg[b])
            reg[c] = 1;
        else
            reg[c] = 0;
        break;
    case 11:
        if (reg[a] > b)
            reg[c] = 1;
        else
            reg[c] = 0;
        break;
    case 12:
        if (reg[a] > reg[b])
            reg[c] = 1;
        else
            reg[c] = 0;
        break;
    case 13:
        if (a == reg[b]) {
            reg[c] = 1;
        } else {
            reg[c] = 0;
        }
        break;
    case 14:
        if (reg[a] == b) {
            reg[c] = 1;
        } else {
            reg[c] = 0;
        }
        break;
    case 15:
        if (reg[a] == reg[b]) {
            reg[c] = 1;
        } else {
            reg[c] = 0;
        }
        break;
    }
}

struct instr {
    int opcode;
    int a;
    int b;
    int c;
};


int main() {
    map<string, int> opcodes = {
        {"addi", 0},
        {"addr", 1},
        {"muli", 2},
        {"mulr", 3},
        {"bani", 4},
        {"banr", 5},
        {"bori", 6},
        {"borr", 7},
        {"seti", 8},
        {"setr", 9},
        {"gtir", 10},
        {"gtri", 11},
        {"gtrr", 12},
        {"eqir", 13},
        {"eqri", 14},
        {"eqrr", 15},
    };

    string discard;
    cin >> discard;  // #ip
    int ip;
    cin >> ip;

    vector<instr> instructions;
    string name;
    while (cin >> name) {
        instr i;
        i.opcode = opcodes[name];
        cin >> i.a;
        cin >> i.b;
        cin >> i.c;
        instructions.push_back(i);
    }

    int ptr = 0;
    uint64_t c = 0;
    vector<int> reg = {1, 0, 0, 0, 0, 0};
    while (ptr >= 0 && ptr < (int) instructions.size()) {
        auto i = instructions[ptr];
        // print(f'ip={ptr} {reg} {opfun} {a} {b} {c}', end=' ')
        // show(reg);
        op(i.opcode, reg, i.a, i.b, i.c);
        // show(reg);
        // print(f'{reg}')
        if (++c % 100'000'000 == 0) {
            show(reg);
        }

        reg[ip]++;
        ptr = reg[ip];
    }

    cout << "part1: ";
    show(reg);

    // reg = [1, 0, 0, 0, 0, 0]
    // reg = [0, 10551327, 10551327, 1, 4, 10551329]
    // reg = [0, 0, 10550400, 10551328, 1, 10551329]
    // reg = [138, 10551330, 1, 10551327, 13, 10551329]
    // # reg = [10551330, 9115049, 0, 10551329, 5, 10551329]
    // # reg = [1, 9541379, 0, 10551328, 10, 10551329]
    // # reg = [1, 7467708, 0, 10551327, 5, 10551329]
    // # reg = [1, 10551329, 0, 10551326, 8, 10551329]
    // ptr = reg[regp]
    // i = 0
    // while ptr in range(len(instructions)):
    //     i += 1
    //     if i % 1000000 == 0:
    //         print(reg)
    //     # if i > 1000000 and reg[3] > 1:
    //     #     print(reg)
    //     #     return
    //     # if reg[3] > 10551000 and i % 1 == 0:
    //     #     print(reg)
    //     # if i > 10000000 and reg[5] - reg[1] > 6 and reg[4] :
    //     #     reg[1] = reg[5] - 6
    //     opfun, a, b, c = instructions[ptr]
    //     opfun(reg, a, b, c)
    //     reg[regp] += 1
    //     ptr = reg[regp]
    // print('part2:', reg)
}
