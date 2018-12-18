import sys


def main():
    recipies = [3, 7]
    a, b = 0, 1
    puz_digits = list(map(int, '540561'))
    puz_len = len(puz_digits)
    print(puz_digits)

    def next_recipie():
        nonlocal a, b
        new_ones = [int(d) for d in str(recipies[a] + recipies[b])]
        for add in new_ones:
            recipies.append(add)
            if recipies[-puz_len:] == puz_digits:
                print(len(recipies) - puz_len)
                sys.exit()
        a = (a + recipies[a] + 1) % len(recipies)
        b = (b + recipies[b] + 1) % len(recipies)

    def show():
        for i in range(len(recipies)):
            if i == a:
                print(f'({recipies[i]})', end=' ')
            elif i == b:
                print(f'[{recipies[i]}]', end=' ')
            else:
                print(f'{recipies[i]}', end=' ')
        print()

    while True:
        # show()
        next_recipie()
    # show()
    # p1 = recipies[puz_input:puz_input+10]
    # print(''.join(map(str, p1)))


main()
