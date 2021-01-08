import sys, os, re
from argparse import ArgumentParser


def parse_string_with_regexp(line, regexp, ignore_case):
    if (ignore_case == True):
        return True if re.search(regexp, line, flags=re.IGNORE_CASE) else False
    else:
        return True if re.search(regexp, line) else False
    

def print_result_regexp(line, pattern, ignore_case, line_number, max_count, i):
    if (parse_string_with_regexp(line, pattern, ignore_case)):
        if (line_number == True):
            print(i, line, sep=': ', end='\n')
        else:
            print(line)
        if (max_count != False):
            max_count[0] -= 1


def parse_string(line, pattern, ignore_case):
    if ignore_case == True:
        pattern = pattern.lower()
        line = line.lower()
    
    return True if pattern in line else False


def print_result(line, pattern, ignore_case, line_number, max_count, i):
    if (parse_string(line, pattern, ignore_case)):
        if (line_number == True):
            print(i, line, sep=': ', end='\n')
        else:
            print(line)
        if (max_count != False):
            max_count[0] -= 1


def parse_file(file_name, pattern, ignore_case, line_number, max_count, regexp):
    text = False
    try:
        text = open(file_name, 'r')
    except Exception as e:
        sys.stderr.write(e.args)
    
    if text != False:
        line = text.readline()
        i = 1

        if (max_count != False):
            if (max_count[0] > 0):
                if (regexp):
                    while line and max_count[0] > 0:
                        print_result_regexp(line, pattern, ignore_case, line_number, max_count, i)
                        i += 1
                        line = text.readline()
                else :
                    while line and max_count[0] > 0:
                        print_result(line, pattern, ignore_case, line_number, max_count, i)
                        i += 1
                        line = text.readline()
        else:
            if (regexp):
                while line:
                    print_result_regexp(line, pattern, ignore_case, line_number, max_count, i)
                    i += 1
                    line = text.readline()
            else:
                while line:
                    print_result(line, pattern, ignore_case, line_number, max_count, i)
                    i += 1
                    line = text.readline()

    text.close()


def parse_files(paths, pattern, ignore_case, line_number, max_count, regexp):
    max_count = [max_count]
    for path in paths:
        if os.path.isfile(path):
            parse_file(path, pattern, ignore_case, line_number, max_count, regexp)
            # print(max_count[0])
        else:
            sys.stderr.write(str(path) + ' is a directory\n')


def set_parser():
    parser = ArgumentParser()
    parser.add_argument('pattern', type=str, default='') # строка для поиска
    parser.add_argument('files', metavar='FILES', nargs='*', default='-') # список файлов
    parser.add_argument('-i', '--ignore-case', action='store_true') # игнорировать регистр
    parser.add_argument('-m', '--max-count', type=int, default=False) # остановиться после x совпавших строк
    parser.add_argument('-n', '--line-number', action='store_true') # печать номера строки вместе с выходными строками
    parser.add_argument('-e', '--regexp', type=str) # передать регулярное выражение
    return parser
 

def main():
    parser = set_parser()
    args = parser.parse_args()
    pattern = args.pattern
    files = [f for f in args.files]
    # print(files)
    # print(args)
    ignore_case = False
    line_number = False
    max_count = False

    if args.ignore_case:
        ignore_case = True
    if args.line_number:
        line_number = True
    if args.max_count != False and args.max_count > 0:
        max_count = args.max_count
    if args.regexp:
        pattern = args.regexp

    parse_files(files, pattern, ignore_case, line_number, max_count, args.regexp)

if __name__ == '__main__':
    main()