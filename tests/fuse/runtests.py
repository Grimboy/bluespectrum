import os, sys, subprocess

sys.setrecursionlimit(1000000)

tests_in_f = open("tests.in")
tests_expected_f = open("tests.expected")

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
    tests = []
    while 1:
        tests.append(get_test())
except StopIteration:
    pass


if not os.path.exists("testdata"):
    os.mkdir("testdata")

os.chdir("testdata")
for test in tests:
    if not os.path.exists(test['comment']):
        os.mkdir(test['comment'])
    os.chdir(test['comment'])
    print test
    # make init.rmh
    init = open("init.asm", 'w')
    init.write("\t.area code (abs)\n")
    init.write("\t.org 0x0000\n")

    init.write("\tld a, #0x%s\n" % test['auxstate_in'][0])
    init.write("\tld i, a\n")
    init.write("\tld a, #0x%s\n" % test['auxstate_in'][1])
    init.write("\tld r, a\n")
    if test['auxstate_in'][2] == "1":
        init.write("\tei\n")
    else:
        init.write("\tdi\n")
    # XXX: Can't support iff2 this way
    init.write("\tim %s\n" % test['auxstate_in'][4])

    init.write("\tld bc, #0x%s\n" % test['regfile_in'][0])
    init.write("\tpush bc\n")
    init.write("\tpop af\n")
    init.write("\tld bc, #0x%s\n" % test['regfile_in'][1])
    init.write("\tld de, #0x%s\n" % test['regfile_in'][2])
    init.write("\tld hl, #0x%s\n" % test['regfile_in'][3])
    init.write("\tex af, af'\n")
    init.write("\texx\n")

    init.write("\tld bc, #0x%s\n" % test['regfile_in'][4])
    init.write("\tpush bc\n")
    init.write("\tpop af\n")
    init.write("\tld bc, #0x%s\n" % test['regfile_in'][5])
    init.write("\tld de, #0x%s\n" % test['regfile_in'][6])
    init.write("\tld hl, #0x%s\n" % test['regfile_in'][7])
    init.write("\tex af, af'\n")
    init.write("\texx\n")

    init.write("\tld ix, #0x%s\n" % test['regfile_in'][8])
    init.write("\tld iy, #0x%s\n" % test['regfile_in'][9])
    init.write("\tld sp, #0x%s\n" % test['regfile_in'][10])
    init.write("\t.db 0xC3, 0x%s, 0x%s\n" % (test['regfile_in'][11][0:2], test['regfile_in'][11][2:4])) # jp **

    init.close()
    subprocess.call(["sdasz80", "-o", "init.rel", "init.asm"])
    subprocess.call(["sdldz80", "-i", "init.ihx", "init.rel"])
    subprocess.call(["python3", "../../ihx2rmh.py", "init.ihx", "16k"])
    # make run.rmh
    prog = open("prog.rmh", 'w')
    for addr, run in test['minit'].items():
        prog.write("@%s\n" % (addr,))
        for word in run:
            prog.write(word + "\n")
        prog.write("76\n")
    prog.close()
    # run the test
    os.chdir("../../../..")
    if os.path.exists("testinit.rmh"):
        os.remove("testinit.rmh")
    os.symlink("tests/fuse/testdata/%s/init.rmh" % (test['comment'],), "testinit.rmh")
    if os.path.exists("testprog.rmh"):
        os.remove("testprog.rmh")
    os.symlink("tests/fuse/testdata/%s/prog.rmh" % (test['comment'],), "testprog.rmh")
    actual_output = subprocess.check_output(["./mkFuseTest"])
    # compare the output
    print actual_output
    break
    os.chdir("tests/fuse/testdata")
