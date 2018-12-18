from collections import defaultdict, namedtuple
import fileinput
import sys

sys.setrecursionlimit(10000)


Coord = namedtuple('Coord', ['y', 'x'])


def show(board):
    for y in range(miny, maxy+1):
        for x in range(minx-1, maxx+2):
            where = Coord(y, x)
            print(board[where], end='')
        print()
    print()


def count(board):
    part1 = part2 = 0
    for where in touched:
        if where.y < miny:
            continue
        what = board[where]
        if what in '|':
            part1 += 1
        if what in '~':
            part1 += 1
            part2 += 1
    return part1, part2


visited = set()
touched = set()


def flood(board, spring):
    if spring in visited:
        return
    visited.add(spring)
    # show(board)
    can_settle = True
    # go left and right
    left = []
    leftf = Coord(spring.y, spring.x)
    while leftf.x >= minx-1 and board[leftf] != '#':
        board[leftf] = '|'
        touched.add(leftf)
        down = Coord(leftf.y+1, leftf.x)
        if board[down] not in '#~' and down.y <= maxy:
            flood(board, down)
        if board[down] not in '#~':
            can_settle = False
            break
        left.append(leftf)
        leftf = Coord(leftf.y, leftf.x - 1)
    can_settle = can_settle and leftf.x >= minx-1
    right = []
    rightf = Coord(spring.y, spring.x)
    while rightf.x <= maxx+1 and board[rightf] != '#':
        board[rightf] = '|'
        touched.add(rightf)
        down = Coord(rightf.y+1, rightf.x)
        if board[down] not in '#~' and down.y <= maxy:
            flood(board, down)
        if board[down] not in '#~':
            can_settle = False
            break
        right.append(rightf)
        rightf = Coord(rightf.y, rightf.x + 1)
    can_settle = can_settle and rightf.x <= maxx+1
    if can_settle:
        for where in left + right:
            board[where] = '~'
            touched.add(where)


def main():
    board = defaultdict(lambda: '.')
    for line in fileinput.input():
        line = line.strip()
        y, x = line.split(', ')
        if y[0] == 'x':
            y, x = x, y
        yx = []
        for val in y, x:
            val = val[2:]
            if '..' in val:
                val = val.split('..')
                val = list(range(int(val[0]), int(val[1])+1))
            else:
                val = [int(val)]
            yx.append(val)
        y, x = yx
        for yy in y:
            for xx in x:
                key = Coord(yy, xx)
                board[key] = '#'
    global maxx, minx, maxy, miny
    maxx = max([where.x for where in board])
    minx = min([where.x for where in board])
    maxy = max([where.y for where in board])
    miny = min([where.y for where in board])
    spring = Coord(0, 500)
    flood(board, spring)
    show(board)
    print(count(board))


main()
