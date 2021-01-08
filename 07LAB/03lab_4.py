#!/usr/bin/python
# -*- coding: UTF-8 -*-

from datetime import datetime

def time_it(func):
    def wrapper(*args):
        start = datetime.now()
        result = func(*args)
        print(datetime.now() - start)
        return result
    return wrapper

def memoize(func):
    memory = {}
    def helper(*args):
        if (args not in memory):
            memory[args] = func(*args)
        return memory[args]
    return helper

@memoize # fib = memoize(fib)
def fib(n):
    if (n == 0): return 0
    if (n == 1): return 1
    if (n == 2): return 1
    else: return fib(n - 1) + fib(n - 2)

def non_memoized_fib(n):
    if (n == 0): return 0
    if (n == 1): return 1
    if (n == 2): return 1
    else: return non_memoized_fib(n - 1) + non_memoized_fib(n - 2)

@time_it
def test_fib(n):
    return fib(n)

@time_it
def test_non_fib(n):
    return non_memoized_fib(n)

print(test_fib(35))
print(test_non_fib(35))
