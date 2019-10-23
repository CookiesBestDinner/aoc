import fileinput


def addr(reg, a, b, c):
    reg[c] = reg[a] + reg[b]


def addi(reg, a, b, c):
    reg[c] = reg[a] + b


def mulr(reg, a, b, c):
    reg[c] = reg[a] * reg[b]


def muli(reg, a, b, c):
    reg[c] = reg[a] * b


def banr(reg, a, b, c):
    reg[c] = reg[a] & reg[b]


def bani(reg, a, b, c):
    reg[c] = reg[a] & b


def borr(reg, a, b, c):
    reg[c] = reg[a] | reg[b]


def bori(reg, a, b, c):
    reg[c] = reg[a] | b


def setr(reg, a, b, c):
    reg[c] = reg[a]


def seti(reg, a, b, c):
    reg[c] = a


def gtir(reg, a, b, c):
    if a > reg[b]:
        reg[c] = 1
    else:
        reg[c] = 0


def gtri(reg, a, b, c):
    if reg[a] > b:
        reg[c] = 1
    else:
        reg[c] = 0


def gtrr(reg, a, b, c):
    if reg[a] > reg[b]:
        reg[c] = 1
    else:
        reg[c] = 0


def eqir(reg, a, b, c):
    if a == reg[b]:
        reg[c] = 1
    else:
        reg[c] = 0


def eqri(reg, a, b, c):
    if reg[a] == b:
        reg[c] = 1
    else:
        reg[c] = 0


def eqrr(reg, a, b, c):
    if reg[a] == reg[b]:
        reg[c] = 1
    else:
        reg[c] = 0


def main():
    ops = {
        'addi':  addi,
        'addr':  addr,
        'muli':  muli,
        'mulr':  mulr,
        'banr':  banr,
        'bani':  bani,
        'borr':  borr,
        'bori':  bori,
        'setr':  setr,
        'seti':  seti,
        'gtir':  gtir,
        'gtri':  gtri,
        'gtrr':  gtrr,
        'eqir':  eqir,
        'eqri':  eqri,
        'eqrr':  eqrr
    }
    once = True
    regp = 0
    instructions = []
    for line in fileinput.input():
        line = line.strip().split()
        if once:
            regp = int(line[1])
            once = False
            continue
        ins = line[0]
        abc = list(map(int, line[1:]))
        instructions.append((ops[ins], *abc))

    ptr = 0
    reg = [0, 0, 0, 0, 0, 0]
    while ptr in range(len(instructions)):
        opfun, a, b, c = instructions[ptr]
        # print(f'ip={ptr} {reg} {opfun} {a} {b} {c}', end=' ')
        opfun(reg, a, b, c)
        # print(f'{reg}')
        reg[regp] += 1
        ptr = reg[regp]
    print('part1:', reg)

    reg = [1, 0, 0, 0, 0, 0]
    reg = [0, 10551327, 10551327, 1, 4, 10551329]
    reg = [0, 0, 10550400, 10551328, 1, 10551329]
    reg = [138, 10551330, 1, 10551327, 13, 10551329]
    # reg = [10551330, 9115049, 0, 10551329, 5, 10551329]
    # reg = [1, 9541379, 0, 10551328, 10, 10551329]
    # reg = [1, 7467708, 0, 10551327, 5, 10551329]
    # reg = [1, 10551329, 0, 10551326, 8, 10551329]
    ptr = reg[regp]
    i = 0
    while ptr in range(len(instructions)):
        i += 1
        if i % 1000000 == 0:
            print(reg)
        # if i > 1000000 and reg[3] > 1:
        #     print(reg)
        #     return
        # if reg[3] > 10551000 and i % 1 == 0:
        #     print(reg)
        # if i > 10000000 and reg[5] - reg[1] > 6 and reg[4] :
        #     reg[1] = reg[5] - 6
        opfun, a, b, c = instructions[ptr]
        opfun(reg, a, b, c)
        reg[regp] += 1
        ptr = reg[regp]
    print('part2:', reg)


main()
