import pytest


class Node:
    def __init__(self, value):
        self.value = value
        self.right = self
        self.left = self

    def goright(self, n):
        result = self
        for _ in range(n):
            result = result.right
        return result

    def goleft(self, n):
        result = self
        for _ in range(n):
            result = result.left
        return result

    def insertAfter(self, other):
        right = self.right
        self.right = other
        right.left = other
        other.left = self
        other.right = right
        return other

    def remove(self):
        self.left.right = self.right
        self.right.left = self.left
        return self.right


def regular(board, n):
    return board.goright(1).insertAfter(Node(n))


def special(board, n):
    back7 = board.goleft(7)
    score = n + back7.value
    return score, back7.remove()


def solve(numplayers, lastvalue):
    scoreboard = [0] * numplayers
    board = Node(0)
    for marble in range(1, lastvalue+1):
        if marble % 23 == 0:
            score, board = special(board, marble)
            player = (marble-1) % numplayers
            scoreboard[player] += score
        else:
            board = regular(board, marble)
    return max(scoreboard)


def main():
    numplayers, lastvalue = [
        int(word) for word in input().split() if word.isdigit()]
    print(solve(numplayers, lastvalue))
    print(solve(numplayers, lastvalue * 100))


if __name__ == '__main__':
    main()


@pytest.mark.parametrize("numplayers, lastvalue, expected", [
    (9, 25, 32),
    (10, 1618, 8317),
    (13, 7999, 146373),
    (17, 1104, 2764),
    (21, 6111, 54718),
    (30, 5807, 37305),
    (419, 71052, 412117),
])
def test_solution(numplayers, lastvalue, expected):
    assert solve(numplayers, lastvalue) == expected
