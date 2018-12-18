from collections import Counter
import time


l1 = input()
_ = input()
__, _, initial_state = l1.split(' ')
initial_state = list(initial_state)

initial_statepad = ['.'] * 10 + initial_state + ['.'] * 250

import fileinput

rules = []
for rule in fileinput.input():
    rule = rule.strip()
    rules.append(rule)


def formatrule(rule):
    state, res = rule.split(' => ')
    return state, res

rules = list(map(formatrule, rules))
rules = {state:res for state, res in rules if res == '#'}

gens = [''.join(initial_statepad)]
offset = -10

seen = {}

for gen in range(1000 * 50):
    prevgen = gens[-1]
    if prevgen not in seen:
        nextgen = ['.'] * 2
        for pos in range(len(prevgen)-4):
            prevstate = prevgen[pos:pos+5]
            changewhere = pos+2
            if prevstate in rules:
                res = rules[prevstate]
            else:
                res = '.'
            nextgen.append(res)
        nextgen.append('.')
        nextgen.append('.')
        nextgen = ''.join(nextgen)
        seen[prevgen] = nextgen
    nextgen = seen[prevgen]
    if nextgen[-6:] != '......':
        nextgen += '.' * 10
        nextgen = nextgen[10:]
        offset += 10
    gens = [nextgen]
    print(nextgen)
    time.sleep(0.111)


total = 0
for i, pot in enumerate(gens[-1], offset):
    if pot == '#':
        total += i

print(total)
