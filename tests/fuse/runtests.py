#!/usr/bin/python2

import os, sys, subprocess, StringIO, atexit

process = None
tests_in_f = open("tests.in")
tests_expected_f = open("tests.expected")

REGFILE_NAMES = ["af", "bc", "de", "hl",
                 "af_", "bc_", "de_", "hl_",
                 "ix", "iy", "sp", "pc"]

AUXSTATE_NAMES = ["i", "r", "iff1", "iff2", "im", "halted", "tstates"]

def get_events(stream):
    events = []
    while 1:
        pos = stream.tell()
        line = stream.readline()
        if len(line) == 0 or line[0] != " ":
            stream.seek(pos)
            break
        bits = line.split()
        bits[0] = int(bits[0])
        bits[2:] = [int(bit, 16) for bit in bits[2:]]
        events.append(bits)
    return events

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
    res['events'] = get_events(tests_expected_f)
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

def run_test(test):
    global process
    if not os.path.exists(test['comment']):
        os.mkdir(test['comment'])
    os.chdir(test['comment'])
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
    subprocess.check_output(["sdldz80", "-i", "init.ihx", "init.rel"])
    subprocess.call(["python2", "../../ihx2rmhnbin.py", "init.ihx", "16k"])
    # make run.rmh
    prog = open("prog.rmh", 'w')
    prog_bin = open("prog.bin", 'wb')
    prog_bin_i = 0
    for addr, run in test['minit'].items():
        addr_n = int(addr, 16)
        while prog_bin_i < addr_n:
            prog.write("76\n")
            prog_bin.write(chr(0x76))
            prog_bin_i += 1
        prog.write("@%s\n" % (addr,))
        for word in run:
            prog.write(word + "\n")
            prog_bin.write(chr(int(word, 16)))
            prog_bin_i += 1
    while prog_bin_i < 0x10000:
        prog.write("76\n")
        prog_bin.write(chr(0x76))
        prog_bin_i += 1
    prog.close()
    prog_bin.close()
    # run the test
    dissassembly = subprocess.check_output(["z80dasm", "-atg", "0", "prog.bin"]).split("\n")
    os.chdir("../../../..")
    if os.path.exists("testinit.rmh"):
        os.remove("testinit.rmh")
    os.symlink("tests/fuse/testdata/%s/init.rmh" % (test['comment'],), "testinit.rmh")
    if os.path.exists("testprog.rmh"):
        os.remove("testprog.rmh")
    os.symlink("tests/fuse/testdata/%s/prog.rmh" % (test['comment'],), "testprog.rmh")

    print "***** TEST %s *****" % (test['comment'],)
    print "Disassembled test:"
    for addr, run in test['minit'].items():
        addr_n = int(addr, 16)
        print addr + ":"
        for line in dissassembly[addr_n+5:]:
            print line
            if line.strip().startswith("halt"):
                break
    print "Initial regfile"
    print test['regfile_in']
    print "Running..."
    process = subprocess.Popen(["./mkFuseTest"], stdout=subprocess.PIPE)
    actual_output, status = process.communicate()
    print "Done"
    if status == -1:
        print "Fail - timed out"
        return False
    actual_output = "\n".join(line[2:] for line in actual_output.split("\n") if line.startswith("**"))
    actual_output_stream = StringIO.StringIO(actual_output)
    actual_events = get_events(actual_output_stream)
    actual_events = [[e[0] - 194] + e[1:] for e in actual_events[65:-1]]
    actual_regfile = actual_output_stream.readline().split()
    print "Expected regfile"
    print test['regfile_out']
    print "Expected events"
    print test['events']
    print "Expected memory"
    print test['mchanged']
    print "Actual regfile"
    print actual_regfile 
    print "Actual events"
    print actual_events
    print "Actual memory"
    dumpfile = open("dram_dump.rmh")
    dump = dumpfile.read().split('\n')
    mactual = {}
    for addr, run in test['mchanged'].items():
        addr_n = int(addr, 16)
        mactual[addr] = []
        for i in xrange(len(run)):
            mactual[addr].append(dump[addr_n + i + 1])
    print mactual
    success = test['regfile_out'][:-1] == actual_regfile[:-1] and test['mchanged'] == mactual
    if success:
        print "Pass"
    else:
        print "Fail"
    os.chdir("tests/fuse/testdata")
    return success

@atexit.register
def kill_subprocess():
    if process:
        try:
            process.kill()
        except OSError:
            pass

tests = []
tests_by_comment = {}

try:
    while 1:
        tests.append(get_test())
except StopIteration:
    pass

for test in tests:
    tests_by_comment[test['comment']] = test

if not os.path.exists("testdata"):
    os.mkdir("testdata")

os.chdir("testdata")

if len(sys.argv) == 1:
    run_tests = tests
else:
    test_comments = sys.argv[1:]
    run_tests = []
    for test_comment in test_comments:
        if '-' in test_comment:
            bits = test_comment.split('-')
            for i in xrange(tests.index(tests_by_comment[bits[0]]), tests.index(tests_by_comment[bits[1]]) + 1):
                run_tests.append(tests[i])
        else:
            run_tests.append(tests_by_comment[test_comment])

successful_tests = []
failed_tests = []
test_count = len(run_tests)
success_count = 0
for test in run_tests:
    if run_test(test):
        success_count += 1
        successful_tests.append(test['comment'])
    else:
        failed_tests.append(test['comment'])

print "#################### Summary ####################"
print ("%s/%s: " % (success_count, test_count)) + ("Success!" if success_count == test_count else "Failed")
print "Sucessful"
print successful_tests
print "Failed"
print failed_tests
