import fileinput
import string

letters = string.ascii_lowercase

lohi = [lo + lo.upper() for lo in letters]
hilo = [lo.upper() + lo for lo in letters]

reactors = lohi+hilo


for line in fileinput.input():
    line = line.strip()
    pass


def reactall(line):
    while True:
        l = len(line)
        for reactor in reactors:
            line = line.replace(reactor, '')
        nl = len(line)
        if l == nl:
            break
    return line

p1indata = line

print('part1:', len(reactall(p1indata)))
results = []
# part2
for char in letters:
    linecopy = line.replace(char, '').replace(char.upper(), '')
    result = len(reactall(linecopy))
    results.append(result)
print('part2:', min(results))
