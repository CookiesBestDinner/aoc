def regular(board, n, current):
    # insert 2 steps ahead if that location exists
    if not len(board):
        insertloc = 0
    else:
        insertloc = (current + 1) % len(board) + 1
    board.insert(insertloc, n)
    return insertloc


def special(board, n, current):
    sevenRemove = (current - 7) % len(board)
    score = n + board[sevenRemove]
    # print('score:', score, 'from:', sevenRemove)
    del board[sevenRemove]
    return score, sevenRemove


def main():
    numplayers, lastvalue = [
        int(word) for word in input().split() if word.isdigit()]
    current = 0
    scoreboard = [0] * numplayers
    board = [0]
    # for marble in range(lastvalue+1):
    for marble in range(1, lastvalue+1):
        if marble % 10000 == 0:
            print(marble)
        player = (marble-1) % numplayers
        if marble != 0 and marble % 23 == 0:
            score, current = special(board, marble, current)
            scoreboard[player] += score
        else:
            current = regular(board, marble, current)
        # print(f'[{player+1}]', current, board)
    print('winner', max(scoreboard))


main()
