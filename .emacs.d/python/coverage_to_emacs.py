#! env /usr/bin/python
import os
import sys
import coverage
import optparse

parser = optparse.OptionParser("%prog path/to/coverage_file module_or_filename.py")

if __name__ == '__main__':
    options, args = parser.parse_args()
    coverage_file, filename = args
    os.chdir(os.path.dirname(coverage_file))
    coverage_data = coverage.coverage(data_file='.coverage')
    coverage_data.load()
    _, _, excluded, missing, _ = coverage_data.analysis2(filename)
    sys.stdout.write("[")
    with open(filename, "rb") as source_file:
        position = 0
        for index, line in enumerate(source_file.xreadlines()):
            index += 1
            if index not in excluded and index in missing:
                sys.stdout.write('[{0}, {1}, "pink"],'.format(position+1, position + len(line)))
            position += len(line)
    sys.stdout.write("]")
