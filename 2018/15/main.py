import fileinput
from collections import deque
from itertools import count

adj_coords = [(0, 1), (0, -1), (1, 0), (-1, 0)]


def heatmap(y, x, board):
    board = [row[:] for row in board]
    board[y][x] = 0
    q = deque()
    q.append((y, x, 0))
    while q:
        (y, x, t) = q.popleft()
        # consider each adjacent '.'
        for dy, dx in adj_coords:
            ny, nx = dy + y, dx + x
            if board[ny][nx] == '.':
                q.append((ny, nx, t+1))
                board[ny][nx] = t+1
    return board


def adjacentLocs(my, mx, units, board):
    result = []
    my_kind = board[my][mx]
    for y, x, kind, hp in units:
        if kind == my_kind or hp <= 0:
            continue
        for dy, dx in adj_coords:
            hy, hx = y+dy, x+dx
            if board[hy][hx] == '.':
                result.append((hy, hx))
    return result


def show(board, tw=1):
    for row in board:
        row = map(str, row)
        row = [f'{tile:^{tw}}' for tile in row]
        print(''.join(row))


def main():
    board = []
    units = []
    for y, line in enumerate(list(fileinput.input())):
        line = line[:-1]
        board.append(list(line))
        for x, tile in enumerate(line):
            if tile in 'GE':
                units.append((y, x, tile, 200))
    numoelves = len([u for u in units if u[2] == 'E'])
    for round in count():
        show(board)
        # remove dead units
        units = [unit for unit in units if unit[3] > 0]
        # order by location into reading order
        units.sort()
        for mi, (my, mx, mkind, mhealth) in enumerate(units):
            if mhealth <= 0:
                continue
            # --------- are there any enemies?
            combat_end = all(unit[2] == mkind for unit in units if unit[3] > 0)
            if combat_end:
                print(f'combat ends during round {round}')
                print(f'units: {[u for u in units if u[3] > 0]}')
                hps = [u[3] for u in units if u[3] > 0]
                print(f'hps: {hps}')
                hpsum = sum(hps)
                print(f'hp sum: {hpsum}')
                part1 = hpsum * round
                print(f'{round} * {hpsum} -> {part1}')
                show(board)
                print(f'number of elves at the start: {numoelves}')
                surviving_elves = len(
                    [u for u in units if u[2] == 'E' and u[3] > 0]
                )
                print(f'total elves alive: {surviving_elves}')
                return
            # --------- should move at all?
            in_range_of_enemy = any(
                tile in 'GE' and tile != mkind
                for tile in
                [board[my+dy][mx+dx] for dy, dx in adj_coords]
            )
            if not in_range_of_enemy:
                # -------- where to move towards
                distances_to = heatmap(my, mx, board)
                moveto_candidates = adjacentLocs(my, mx, units, board)
                # which of those is nearest?
                wheretogo = []
                for cy, cx in moveto_candidates:
                    dist = distances_to[cy][cx]
                    if isinstance(dist, int):  # is reachable at all
                        wheretogo.append((dist, cy, cx))
                if wheretogo:
                    _, ty, tx = min(wheretogo)
                    # now find the path there
                    distances_from = heatmap(ty, tx, board)
                    # out of my adjacent locations, which is smallest?
                    myadj = []
                    for dy, dx in adj_coords:
                        wy, wx = my + dy, mx + dx
                        if isinstance(distances_from[wy][wx], int):
                            myadj.append((distances_from[wy][wx], wy, wx))
                    # next single step is:
                    _, ny, nx = min(myadj)
                    # ---------- move
                    # current pos is now .
                    board[my][mx] = '.'
                    # and, move
                    board[ny][nx] = mkind
                    # update own coords, and also in units
                    my, mx = ny, nx
                    units[mi] = (my, mx, mkind, mhealth)
            # --------- attack phase
            # look at adjacent tiles
            attackme = []
            for dy, dx in adj_coords:
                ay, ax = my + dy, mx + dx
                # is enemy?
                atile = board[ay][ax]
                if atile in 'GE' and atile != mkind:
                    attackme.append((ay, ax))
            # find their locations in units
            adj_enemies = []
            for ui, (uy, ux, ukind, uhp) in enumerate(units):
                if (uy, ux) in attackme and uhp > 0:
                    adj_enemies.append((uhp, uy, ux, ukind, ui))
            if adj_enemies:
                thp, ty, tx, tkind, ti = min(adj_enemies)
                if mkind == 'G':
                    thp -= 3
                else:
                    # for part 2: change 3 to minimum value that keeps all
                    # elves alive
                    thp -= 3
                units[ti] = (ty, tx, tkind, thp)
                if thp <= 0:
                    board[ty][tx] = '.'


main()
