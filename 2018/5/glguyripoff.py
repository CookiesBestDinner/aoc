from string import ascii_lowercase as letters

line = raw_input()


def react(line):
    line = [ord(ch) for ch in line]
    acc = []
    while line:
        acc.append(line.pop())
        if len(acc) >= 2:
            a, b = acc[-1], acc[-2]
            if a ^ b == 0x20:
                acc[-2:] = []
    return acc


def main():
    reacted = ''.join([chr(i) for i in react(line)])
    print('part1:', len(reacted))
    results = []
    # part2
    for char in letters:
        linecopy = reacted.replace(char, '').replace(char.upper(), '')
        result = len(react(linecopy))
        results.append(result)
    print('part2:', min(results))


main()
