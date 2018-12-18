import fileinput


def display(grid, carts):
    grid = [line[:] for line in grid]
    for dir, memory, y, x in carts:
        grid[y][x] = dir
    grid = grid[::-1]
    for line in grid:
        print(''.join(line))


def fcoord(y, x):
    alty = size - 1 - y
    return f'({x}, {alty})'


def fcart(cart):
    dir, mem, y, x = cart
    coord = fcoord(y, x)
    return f'{dir}, {mem}, {coord}'


def cartpos(cart):
    dir, mem, y, x = cart
    return (-y, x)


def main():
    grid = []

    for line in fileinput.input():
        line = line[:-1]
        grid.append(list(line))
    global size
    size = len(grid)

    grid = grid[::-1]
    # locate all the carts
    cart_symbols = '^><v'
    carts = []

    for y, row in enumerate(grid):
        for x, tile in enumerate(row):
            if tile in cart_symbols:
                carts.append((tile, 0, y, x))
                if tile in '^v':
                    grid[y][x] = '|'
                else:
                    grid[y][x] = '-'
    tick = 0
    while True:
        tick += 1
        # display(grid, carts)
        # print(list(map(fcart, carts)))
        if len(carts) <= 1:
            dir, memory, y, x = carts[0]
            print('end of the line!', fcoord(y, x))
            print(tick)
            return
        newcarts = []
        # for dir, memory, y, x in carts:
        carts = sorted(carts, key=cartpos)[::-1]
        # print(list(map(fcart, carts)))
        # print(carts)

        while carts:
            dir, memory, y, x = carts.pop()
            # cart = (dir, memory, y, x)
            if dir == '^':
                y += 1
            if dir == 'v':
                y -= 1
            if dir == '>':
                x += 1
            if dir == '<':
                x -= 1
            tile = grid[y][x]
            # print(cart, tile)
            if tile == '+':
                if memory == 0:
                    kitty = '^<v>^'
                    dir = kitty[kitty.index(dir) + 1]
                    memory = 1
                elif memory == 1:
                    # go straight
                    memory = 2
                elif memory == 2:
                    kitty = '^>v<^'
                    dir = kitty[kitty.index(dir) + 1]
                    memory = 0
            if tile == '/':
                if dir == '^':
                    dir = '>'
                elif dir == '<':
                    dir = 'v'
                elif dir == 'v':
                    dir = '<'
                elif dir == '>':
                    dir = '^'
            if tile == '\\':
                if dir == '^':
                    dir = '<'
                elif dir == '>':
                    dir = 'v'
                elif dir == 'v':
                    dir = '>'
                elif dir == '<':
                    dir = '^'
            newcart = (dir, memory, y, x)

            crash = False
            nc = []
            for ydir, ymem, yy, yx in carts:
                if not (yy == y and yx == x):
                    nc.append((ydir, ymem, yy, yx))
                else:
                    crash = True
            carts = nc

            nc = []
            for ydir, ymem, yy, yx in newcarts:
                if not (yy == y and yx == x):
                    nc.append((ydir, ymem, yy, yx))
                else:
                    crash = True
            newcarts = nc

            if crash:
                print('crash at', fcoord(y, x))
                continue
            newcarts.append(newcart)

        carts = newcarts


main()
