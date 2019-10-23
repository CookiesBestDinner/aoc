from hashlib import md5
from itertools import count


def main():
    secret = input().encode()
    ns = count()
    result = next(filter(verify(secret), ns))
    print(result)

    ns = count()
    result = next(filter(verify2(secret), ns))
    print(result)


def verify(secret):
    def _verify(n):
        hash = md5(secret + str(n).encode()).hexdigest()
        return hash[:5] == "00000"
    return _verify

def verify2(secret):
    def _verify(n):
        hash = md5(secret + str(n).encode()).hexdigest()
        return hash[:6] == "000000"
    return _verify


main()
