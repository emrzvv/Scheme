import sys, os, re

def tokenize(file):
    results = []
    try:
        with open(file) as f:
            if not os.path.isfile(file):
                sys.stderr.write(str(file) + ' is a directory\n')
            else:
                line_index = 1
                for line in f:
                    for lexeme in re.finditer(r'\S+', line):
                        index, item = lexeme.start(), lexeme.group().replace(',', '')
                        results.append([line_index, index + 1, item])
                        #print(index, item.replace(',', ''), sep=' ')
                    line_index += 1
            
    except Exception as e:
        print(e.args)
    return results


def get_dictionary(file):
    d = {}
    try:
        with open(file) as f:
            if not os.path.isfile(file):
                sys.stderr.write(str(file) + ' is a directory\n')
            else:
                for line in f:
                    for word in re.finditer(r'\S+', line):
                        item = word.group().replace(',', '')
                        d.update({item: 1})
    except Exception as e:
        print(e.args)
    return d


def main():
    #print(sys.argv)
    dictionary = get_dictionary(sys.argv[1])
    tokens = tokenize(sys.argv[2])

    for item in tokens:
        if dictionary.get(item[2]) == None:
            print(f'{item[0]}, {item[1]}\t{item[2]}')



if __name__ == '__main__':
    main()