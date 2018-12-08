# $ time python main.py < zdata
# number of overlaps 111935
# 650 has no overlap
#
# real    0m0.494s
# user    0m0.457s
# sys     0m0.033s

import collections
import fileinput


def main():
    # parse
    claims = []
    for claim in fileinput.input():
        claim = claim.strip().split()
        [col, row] = map(int, claim[2][:-1].split(','))
        [width, height] = map(int, claim[3].split('x'))
        claims.append((col, row, width, height))

    # for each claim: add +1 to each location covered
    board = collections.defaultdict(int)
    for i, [cc, cr, cw, ch] in enumerate(claims):
        for x in range(cc, cc+cw):
            for y in range(cr, cr+ch):
                board[(y, x)] += 1

    # part1: count overlaps (locations covered by more than 1 claim)
    count = 0
    for p in board.values():
        if p > 1:
            count += 1
    print('number of overlaps', count)

    # part2: find claim without overlap (all its locations == 1)
    for i, [cc, cr, cw, ch] in enumerate(claims):
        overlap = False
        for x in range(cc, cc+cw):
            for y in range(cr, cr+ch):
                if board[(y, x)] > 1:
                    overlap = True
        if not overlap:
            print(i+1, 'has no overlap')


main()
