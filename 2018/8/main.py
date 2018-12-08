class Node:
    def __init__(self, children, meta):
        self.children = children
        self.meta = meta

    def sum(self):
        return sum(self.meta) + sum(child.sum() for child in self.children)

    def value(self):
        if not self.children:
            return sum(self.meta)
        indices = [n-1 for n in self.meta]
        indices = [i for i in indices if i in range(len(self.children))]
        kids = [self.children[i] for i in indices]
        value = sum([child.value() for child in kids])
        return value


def main():
    data = map(int, input().split())
    tree = parse(iter(data))
    print(tree.sum())
    print(tree.value())


def parse(ns):
    numchildren = next(ns)
    nummeta = next(ns)
    children = []
    for _child in range(numchildren):
        children.append(parse(ns))
    meta = []
    for _meta in range(nummeta):
        meta.append(next(ns))
    return Node(children, meta)


main()
