import sys

sys.setrecursionlimit(1000000)

tests_in = open(sys.argv[1])

REGFILE_NAMES = ["af", "bc", "de", "hl",
                 "af_", "bc_", "de_", "hl_",
                 "ix", "iy", "sp", "pc"]

AUXSTATE_NAMES = ["i", "r", "iff1", "iff2", "im", "halted", "tstates"]

def get_test():
    res = {}
    while 1:
        comment = tests_in.next()
        if comment != '\n':
            res['comment'] = comment[:-1]
            break
    res['regfile'] = tests_in.next().split()
    res['auxstate'] = tests_in.next().split()
    res['minit'] = {}
    while 1:
        line = tests_in.next()
        if line.startswith('-1'):
            break
        line_bits = line.split()[:-1]
        res['minit'][line_bits[0]] = line_bits[1:]
    return res

def h(x, b=16):
    return "%d'h%s" % (b, x)

def list2consstr(l, s):
    if len(l) == 0:
        return "Nil"
    else:
        return "cons(" + s(l[0]) + ", " + list2consstr(l[1:], s) + ")"

def fmt_minit_run(run):
    payload = list2consstr(run[1], lambda y: h(y, 8))
    return "MemoryInitRunT{startaddr: %s, payload: %s}" % (h(run[0]), payload)

def fmt_test(test):
    return """\nTestT{comment: "%s", %s, %s, events: Nil, minit: %s}""" % (
        test['comment'], 
        ', '.join(x + ': ' + h(y) for x, y in zip(REGFILE_NAMES, test['regfile'])), 
        ', '.join(y[0] + ': ' + (h(y[1], 8) if (x < 2) else y[1]) for x, y in enumerate(zip(AUXSTATE_NAMES, test['auxstate']))), 
        list2consstr(test['minit'].items(), fmt_minit_run)
    )

try:
    tests = []
    while 1:
        tests.append(get_test())
except StopIteration:
    pass

print "List#(TestT) tests = %s;" % (list2consstr(tests, fmt_test),)
