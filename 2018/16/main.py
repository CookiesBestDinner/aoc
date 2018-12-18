from ast import literal_eval
import constraint
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
    ops = [addi, addr,
           muli, mulr,
           banr, bani,
           borr, bori,
           setr, seti,
           gtir, gtri, gtrr,
           eqir, eqri, eqrr,
           ]

    opcodesp1 = {opcode: ops[:] for opcode in range(16)}
    opcodes = {opcode: ops[:] for opcode in range(16)}
    lines = list(fileinput.input())[::-1]
    lines = [line[:-1] for line in lines]
    p1 = 0
    while lines:
        beforeline = lines.pop()
        if not beforeline:
            break
        before = literal_eval(''.join(beforeline.split()[1:]))
        regline = lines.pop()
        op = list(map(int, regline.split(' ')))
        afterline = lines.pop()
        after = literal_eval(''.join(afterline.split()[1:]))
        lines.pop()  # delimiting blank line

        opcodes[op[0]] = limitop(opcodes[op[0]], before, op, after)
        remaining = opcodesp1[op[0]]
        if len(remaining) >= 3:
            p1 += 1

    p = constraint.Problem()
    p.addConstraint(constraint.AllDifferentConstraint())
    for opcode, ops in opcodes.items():
        p.addVariable(opcode, ops)
    opcodes = p.getSolution()

    print('part1:', p1)
    while not lines[-1]:
        lines.pop()
    instructions = lines[::-1]
    reg = [0, 0, 0, 0]
    for ins in instructions:
        opcode, a, b, c = map(int, ins.split(' '))
        opcodes[opcode](reg, a, b, c)
    print('part2:', reg)


def limitop(candidates, before, op, after):
    matched = []
    opcode, a, b, c = op
    for candop in candidates:
        reg = before[:]
        candop(reg, a, b, c)
        if reg == after:
            matched.append(candop)
    return matched


main()
