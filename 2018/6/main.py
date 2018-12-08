from collections import Counter
import fileinput


def get_dist(a, b):
    ax, ay = a
    bx, by = b
    return abs(ax - bx) + abs(ay - by)


def getkey(thing):
    who, howlong = thing
    return howlong


def main():
    coords = []
    for line in fileinput.input():
        line = line.strip()
        x, y = map(int, line.split(', '))
        coords.append((x, y))
    max_x = max([x for x, y in coords])
    max_y = max([y for x, y in coords])

    grid = [
        [None for col in range(max_x+2)]
        for row in range(max_y+2)
    ]

    for y, row in enumerate(grid):
        for x, col in enumerate(row):
            distances = []
            for which_coord, (cx, cy) in enumerate(coords):
                dist = get_dist((x, y), (cx, cy))
                distances.append((which_coord, dist))
            winner = min(distances, key=getkey)
            who, howlong = winner
            count = len([1 for _, howlong2
                         in distances
                         if howlong2 == howlong])
            if count == 1:
                grid[y][x] = who

    disqualified = set()
    for v in grid[0]:
        disqualified.add(v)
    for v in grid[-1]:
        disqualified.add(v)
    for row in grid:
        disqualified.add(row[0])
    for row in grid:
        disqualified.add(row[-1])

    concatted = []
    for row in grid:
        for col in row:
            concatted.append(col)
    counts = Counter(concatted)
    for dis in disqualified:
        if dis in counts:
            del counts[dis]

    print(counts)
    print(counts.most_common())
    import sys; sys.exit()

    def getalldists(fromm):
        total = 0
        for coord in coords:
            total += get_dist(fromm, coord)
        return total

    count = 0
    for x in range(-500, 1200):
        for y in range(-500, 1000):
            totdist = getalldists((x, y))
            if totdist < 10000:
                count += 1
    print('regionthing:', count)


main()
