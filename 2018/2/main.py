import collections
import fileinput
import functools
import itertools
import math
import operator

######### part1
# twos = 0
# threes = 0
# for line in fileinput.input():
#     counts = collections.Counter(line)
#     has_two = any(amount == 2 for _, amount in counts.items())
#     has_three = any(amount == 3 for _, amount in counts.items())
#     if has_three:
#         threes += 1
#     if has_two:
#         twos += 1
#
#
# print(twos * threes)




# part2
data = []
twos = 0
threes = 0
for line in fileinput.input():
    data.append(line)

for a in data:
    for b in data:
        if a == b: continue
        diff = 0
        for i in range(len(a)):
            if a[i] != b[i]:
                diff += 1
        if diff == 1:
            print(a)
            print(b)
