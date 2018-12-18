from functools import lru_cache
data = 7165
# data = 42
# data = 18


@lru_cache(maxsize=400*400)
def powercell(y, x):
    rackid = (1+x) + 10
    powerLevel = rackid * (1+y)
    powerLevel += data
    powerLevel *= rackid
    powerLevel = (powerLevel // 100) % 10
    powerLevel -= 5
    return powerLevel


grid = [[0] * 300] * 300
for y in range(300):
    for x in range(300):
        grid[y][x] = powercell(y+1, x+1)


def getScore(y, x, grid):
    total = 0
    for dy in range(y, y+3):
        if dy >= 300:
            return -float('inf')
        for dx in range(x, x+3):
            if dx >= 300:
                return -float('inf')
            total += powercell(dy, dx)
    return total


def subs(y, x, grid, s):
        total = 0
        for dy in range(y, y+s):
            if dy >= 300:
                return -float('inf')
            for dx in range(x, x+s):
                if dx >= 300:
                    return -float('inf')
                total += powercell(dy, dx)
        return total


def getScore2(y, x, grid):
    bestsize = None
    howmuch = None
    # for s in range(16, 17):
    for s in range(1, 301):
        total = subs(y, x, grid, s)
        if howmuch is None or total > howmuch:
            howmuch = total
            bestsize = s
    return howmuch, bestsize


best = None
bs = 0

for y in range(300):
    if y % 3 == 0:
        print(y)
    for x in range(300):
        score, size = getScore2(y, x, grid)
        if best is None or score > bs:
            bs = score
            best = (y, x, size)

print(best, bs)
y, x, s = best
print('ans:', x+1, y+1, s)
