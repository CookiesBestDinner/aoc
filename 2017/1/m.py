data = input()
total = 0
# offset=1 for part1
offset = len(data) // 2
for i, digit in enumerate(data):
    if data[i-offset] == digit:
        total += int(digit)

print(total)
