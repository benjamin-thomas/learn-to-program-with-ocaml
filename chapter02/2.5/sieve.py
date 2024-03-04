import os
import math

# echo ./sieve.py | entr -c bash -c 'python3 -m doctest ./sieve.py && echo OK'
# RL=1 python3 ./sieve.py


def get_max():
    return int(input("Enter a number: "))


def sieve(max_, prime):
    """
    Use the sieve of Eratosthenes to keep only prime numbers from an array.

    For example, items at position 2,3,5,7 remain True, others are set to False.
    >>> sieve(10, [True] * 11)
    [False, False, True, True, False, True, False, True, False, False, False]
    """
    limit = int(math.sqrt(max_))
    prime[0] = False
    prime[1] = False

    for n in range(2, limit + 1):
        if prime[n]:
            # breakpoint()
            m = n * n
            while m <= max_:
                prime[m] = False
                m += n

    return prime


def main():
    max_ = get_max() if os.getenv("RL") == "1" else 25
    prime = [True] * (max_+1)
    sieve(max_, prime)
    for n in range(2, max_):
        if prime[n]:
            print(n)


main()
