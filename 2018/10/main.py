import fileinput
import matplotlib.pyplot as plt


class Point:
    def advance(self):
        self.x += self.dx
        self.y += self.dy


def show(xs, ys):
    plt.scatter(xs, ys)
    plt.show()


def ticks(points):
    i = 0
    while True:
        ys = [-p.y for p in points]
        diffy = max(ys) - min(ys)
        if diffy < 10:
            xs = [p.x for p in points]
            print(i)
            show(xs, ys)
            for p in points:
                print(p.x, p.y)
            return
        for p in points:
            p.advance()
        i += 1


def main():
    points = []
    for line in fileinput.input():
        line = line.strip()
        p = Point()
        p.x = int(line[10:16])
        p.y = int(line[18:24])
        p.dx = int(line[36:38])
        p.dy = int(line[40:42])
        points.append(p)
    ticks(points)


main()
