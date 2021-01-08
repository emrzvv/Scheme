import sys, os, re
from argparse import ArgumentParser

def get_size(filename):
    return os.stat(filename).st_size

def wc(filenames):
    results = {}
    for filename in filenames:
        if not os.path.isfile(filename):
            sys.stderr.write(str(filename) + ' is a directory\n')
            continue
        chars = 0
        words = 0
        lines = 0
        try:
            with open(filename) as f:
                for line in f:
                    lines += 1
                    words += len(line.split())
                    chars += len(line)
            results[filename] = {
                'lines': lines,
                'words': words,
                'chars': chars,
                'bytes': get_size(filename),
            }
        except Exception as e:
            print(e.args)
    return results
 
def set_parser():
    parser = ArgumentParser()
    parser.add_argument('files', metavar='FILES', nargs='*', default='-') # список файлов
    parser.add_argument('-c', '--bytes', action='store_true') # напечатать количество байт
    parser.add_argument('-m', '--chars', action='store_true') # напечатать количество символов
    parser.add_argument('-w', '--words', action='store_true') # напечатать количество слов
    parser.add_argument('-l', '--lines', action='store_true') # напечатать количество строк
    return parser
 
def main():
    parser = set_parser()
    args = parser.parse_args()
    files = [f for f in args.files]
    results = wc(files)
    totals = {
        'lines': 0,
        'words': 0,
        'chars': 0,
        'bytes': 0,
    }
    for filename in results:
        res = results[filename]
        
        if len(sys.argv) <= 2:
            print(f'{res["lines"]} {res["words"]} {res["chars"]} {res["bytes"]} {filename}')
        else:
            print(filename, end=': ')
            if args.lines:
                print(f'Lines: {res["lines"]}', end=' ')
            if args.words:
                print(f'Words: {res["words"]}', end=' ')
            if args.chars:
                print(f'Chars: {res["chars"]}', end=' ')
            if args.bytes:
                print(f'Bytes: {res["bytes"]}')
            print()
        
        for key in res:
            totals[key] += res[key]
    
    if len(sys.argv) <= 2:
        print(f'{totals["lines"]} {totals["words"]} {totals["chars"]} {totals["bytes"]} total')
    else:
        print('totals: ', end=' ')
        if args.lines:
            print(f'Lines: {totals["lines"]}', end=' ')
        if args.words:
            print(f'Words: {totals["words"]}', end=' ')
        if args.chars:
            print(f'Chars: {totals["chars"]}', end=' ')
        if args.bytes:
            print(f'Bytes: {totals["bytes"]}', end=' ')
        

if __name__ == '__main__':
    main()