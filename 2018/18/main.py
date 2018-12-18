import fileinput
from collections import Counter


def show(board):
    for row in board:
        print(''.join(row))
    print()


def count(board):
    return Counter(''.join(''.join(row) for row in board))


def score(board):
    counts = count(board)
    return counts['#'] * counts['|']


def adjacent(board, y, x):
    adj = []
    ybound = range(len(board))
    xbound = range(len(board[0]))
    for dx in range(-1, 2):
        # if dx+x not in range(0, len(board[0])):
        if dx+x not in xbound:
            continue
        for dy in range(-1, 2):
            if dx == dy == 0:
                continue
            if dy+y not in ybound:
                continue
            tile = board[y+dy][x+dx]
            adj.append(tile)
    return adj


def tick(board):
    newboard = [row[:] for row in board]
    for y in range(len(board)):
        for x in range(len(board[y])):
            adj = adjacent(board, y, x)
            # An open acre will become filled with trees if three or more
            # adjacent acres contained trees. Otherwise, nothing happens.
            if board[y][x] == '.':
                if adj.count('|') >= 3:
                    newboard[y][x] = '|'
            # An acre filled with trees will become a lumberyard if three or
            # more adjacent acres were lumberyards. Otherwise, nothing
            # happens.
            elif board[y][x] == '|':
                if adj.count('#') >= 3:
                    newboard[y][x] = '#'
            # An acre containing a lumberyard will remain a lumberyard if it
            # was adjacent to at least one other lumberyard and at least one
            # acre containing trees. Otherwise, it becomes open.
            elif board[y][x] == '#':
                if not ('#' in adj and '|' in adj):
                    newboard[y][x] = '.'
    return newboard


board = []
for line in fileinput.input():
    line = line.strip()
    board.append(list(line))
# show(board)

seen = set()
once = True

c = 0
while True:
    key = ''.join(''.join(row) for row in board)
    # iterate until it starts repeating
    if key in seen:
        # then clear the seen states
        if once:
            print(f'encounterd a repeat state after {c} ticks')
            once = False
            seen = set()
        # keep going one more cycle to count its length
        else:
            print(f'recorded whole cycle after a total of {c} ticks')
            break
    seen.add(key)
    board = tick(board)
    c += 1
    # oh and part 1 wants to know what the score is after 10 iterations
    # uh. theoretically this might not happen so it's kind of a bug but for my
    # input this loop does run at least 10 times..
    if c == 10:
        print(f'part1: {score(board)}')

# skip ahead a multiple of the cycle (state remains unchanged)
cycle = len(seen)
skips = (1000000000 - c) // cycle
c += skips * cycle

# finish looping until 1b ticks have happened
while c < 1000000000:
    board = tick(board)
    c += 1


print('part2:', score(board))
