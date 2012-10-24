import sys

sys.setrecursionlimit(1000000)

tests_in_f = open(sys.argv[1])
tests_expected_f = open(sys.argv[2])

REGFILE_NAMES = ["af", "bc", "de", "hl",
                 "af_", "bc_", "de_", "hl_",
                 "ix", "iy", "sp", "pc"]

AUXSTATE_NAMES = ["i", "r", "iff1", "iff2", "im", "halted", "tstates"]

def get_test():
    res = {}

    # tests.in
    while 1:
        comment = tests_in_f.readline()
        if comment != '\n':
            res['comment'] = comment[:-1]
            break
    res['regfile_in'] = tests_in_f.readline().split()
    res['auxstate_in'] = tests_in_f.readline().split()
    res['minit'] = {}
    while 1:
        line = tests_in_f.readline()
        if line.startswith('-1'):
            break
        elif line == "":
            raise StopIteration
        line_bits = line.split()[:-1]
        res['minit'][line_bits[0]] = line_bits[1:]

    # tests.expected
    while 1:
        comment = tests_expected_f.readline()
        if comment != '\n':
            sys.stdout.flush()
            assert comment[:-1] == res['comment']
            break
    res['events'] = []
    while 1:
        pos = tests_expected_f.tell()
        line = tests_expected_f.readline()
        if line[0] != " ":
            tests_expected_f.seek(pos)
            break
        bits = line.split()
        bits.append("00")
        res['events'].append(bits)
    res['regfile_out'] = tests_expected_f.readline().split()
    res['auxstate_out'] = tests_expected_f.readline().split()
    res['mchanged'] = {}
    while 1:
        line = tests_expected_f.readline()
        if line == '\n':
            break
        line_bits = line.split()[:-1]
        sys.stdout.flush()
        res['mchanged'][line_bits[0]] = line_bits[1:]
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
    return "MemoryRunT{startaddr: %s, payload: %s}" % (h(run[0]), payload)

def fmt_minit_run_list(run_list):
    return list2consstr(run_list.items(), fmt_minit_run)

def fmt_state(regfile, auxstate):
    return "Z80StateT{%s, %s}" % (
        ', '.join(x + ': ' + h(y) for x, y in zip(REGFILE_NAMES, regfile)), 
        ', '.join(y[0] + ': ' + (h(y[1], 8) if (x < 2) else y[1]) for x, y in enumerate(zip(AUXSTATE_NAMES, auxstate))), 
    )

def fmt_event(event):
    return "EventT{time_: %s, type_: Ev%s, addr: %s, data: %s}" % (
        event[0],
        event[1],
        h(event[2]),
        h(event[3], 8),
    )

def fmt_test(test):
    return """\nTestT{comment: "%s", state_in: %s, minit: %s, state_out: %s, events: %s, mchanged: %s}""" % (
        test['comment'], 
        fmt_state(test['regfile_in'], test['auxstate_in']),
        fmt_minit_run_list(test['minit']),
        fmt_state(test['regfile_out'], test['auxstate_out']),
        list2consstr(test['events'], fmt_event),
        fmt_minit_run_list(test['mchanged']),
    )

try:
    tests_in = []
    while 1:
        tests_in.append(get_test())
except StopIteration:
    pass

print "List#(TestT) tests_in = %s;" % (list2consstr(tests_in, fmt_test),)
