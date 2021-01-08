import sys
import random

def program():
    symbols = '1234567890-=qwertyuiopQWERTYUIOP[]{}asdfghjklASDFGHJKL;\'zxcvbnmZXCVBNM,./!@#$%^&*()_+<>`~'
    if (len(sys.argv) == 3):
        amount = int(sys.argv[1])
        length = int(sys.argv[2])
        mod = len(symbols)
        for i in range(0, amount):
            for j in range(0, length):
                print(symbols[random.randint(0, length - 1)], end='')
            print()
    else:
        print("Wrong argument input\n")

program()