#!/usr/bin/python2.7
# xdelta3 - delta compression tools and library -*- Mode: C++ -*-
# Copyright 2016 Joshua MacDonald
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# TODO This code is no longer maintained :(

import os, sys, math, re, time, types, array, random
import xdelta3

RCSDIR = '/tmp/rcs'
SAMPLEDIR = "/tmp/diff"

#
MIN_SIZE       = 0

TIME_TOO_SHORT = 0.050

SKIP_TRIALS    = 2
MIN_TRIALS     = 3
MAX_TRIALS     = 15

# 10 = fast 1.5 = slow
MIN_STDDEV_PCT = 1.5

# How many results per round
MAX_RESULTS = 500
TEST_ROUNDS = 10
KEEP_P = (0.5)

# For RCS testing, what percent to select
FILE_P = (0.50)

# For run-speed tests
MIN_RUN = 1000 * 1000 * 1
MAX_RUN = 1000 * 1000 * 10

# Testwide defaults
ALL_ARGS = [
    '-q'  # '-vv'
    ]

# The first 7 args go to -C
SOFT_CONFIG_CNT = 7

CONFIG_ORDER = [ 'large_look',
                 'large_step',
                 'small_look',
                 'small_chain',
                 'small_lchain',
                 'max_lazy',
                 'long_enough',

                 # > SOFT_CONFIG_CNT
                 'nocompress',
                 'winsize',
                 'srcwinsize',
                 'sprevsz',
                 'iopt',
                 'djw',
                 'altcode',
                 ]

CONFIG_ARGMAP = {
    'winsize'    : '-W',
    'srcwinsize' : '-B',
    'sprevsz'    : '-P',
    'iopt'       : '-I',
    'nocompress' : '-N',
    'djw'        : '-Sdjw',
    'altcode'    : '-T',
    }

def INPUT_SPEC(rand):
    return {

    # Time/space costs:

    # -C 1,2,3,4,5,6,7
    'large_look' : lambda d: rand.choice([9, 10, 11, 12]),
    'large_step' : lambda d: rand.choice([25, 26, 27, 28, 29, 30]),
    'small_look'   : lambda d: rand.choice([4]),
    'small_chain'  : lambda d: rand.choice([1]),
    'small_lchain' : lambda d: rand.choice([1]),
    'max_lazy'     : lambda d: rand.choice([4, 5, 6, 7, 8, 9, 10 ]),

    # Note: long_enough only refers to small matching and has no effect if
    # small_chain == 1.
    'long_enough'  : lambda d: rand.choice([4]),

    # -N
    'nocompress'   : lambda d: rand.choice(['false']),

    # -T
    'altcode'      : lambda d: rand.choice(['false']),

    # -S djw
    'djw'          : lambda d: rand.choice(['false']),

    # Memory costs:

    # -W
    'winsize'      : lambda d: 8 * (1<<20),

    # -B
    'srcwinsize'   : lambda d: 64 * (1<<20),

    # -I 0 is unlimited
    'iopt'         : lambda d: 0,

    # -P only powers of two
    'sprevsz'      : lambda d: rand.choice([x * (1<<16) for x in [4]]),
  }
#end

#
TMPDIR = '/tmp/xd3regtest.%d' % os.getpid()

RUNFILE = os.path.join(TMPDIR, 'run')
DFILE   = os.path.join(TMPDIR, 'output')
RFILE   = os.path.join(TMPDIR, 'recon')
CMPTMP1 = os.path.join(TMPDIR, 'cmptmp1')
CMPTMP2 = os.path.join(TMPDIR, 'cmptmp2')

HEAD_STATE = 0
BAR_STATE  = 1
REV_STATE  = 2
DATE_STATE = 3

#
IGNORE_FILENAME  = re.compile('.*\\.(gif|jpg).*')

# rcs output
RE_TOTREV  = re.compile('total revisions: (\\d+)')
RE_BAR     = re.compile('----------------------------')
RE_REV     = re.compile('revision (.+)')
RE_DATE    = re.compile('date: ([^;]+);.*')
# xdelta output
RE_HDRSZ   = re.compile('VCDIFF header size: +(\\d+)')
RE_EXTCOMP = re.compile('XDELTA ext comp.*')

def c2str(c):
    return ' '.join(['%s' % x for x in c])
#end

def SumList(l):
    return reduce(lambda x,y: x+y, l)
#end

# returns (total, mean, stddev, q2 (median),
#          (q3-q1)/2 ("semi-interquartile range"), max-min (spread))
class StatList:
    def __init__(self,l,desc):
        cnt = len(l)
        assert(cnt > 1)
        l.sort()
        self.cnt    = cnt
        self.l      = l
        self.total  = SumList(l)
        self.mean   = self.total / float(self.cnt)
        self.s      = math.sqrt(SumList([(x-self.mean) * 
                                         (x - self.mean) for x in l]) / 
                                float(self.cnt-1))
        self.q0     = l[0]
        self.q1     = l[int(self.cnt/4.0+0.5)]
        self.q2     = l[int(self.cnt/2.0+0.5)]
        self.q3     = l[min(self.cnt-1,int((3.0*self.cnt)/4.0+0.5))]
        self.q4     = l[self.cnt-1]
        self.siqr   = (self.q3-self.q1)/2.0;
        self.spread = (self.q4-self.q0)
        if len(l) == 1:
            self.str = '%s %s' % (desc, l[0])
        else:
            self.str = '%s mean %.1f: 25%-ile %d %d %d %d %d' % \
                (desc, self.mean, self.q0, self.q1, self.q2, self.q3, self.q4)
    #end
#end

def RunCommand(args, ok = [0]):
    #print 'run command %s' % (' '.join(args))
    p = os.spawnvp(os.P_WAIT, args[0], args)
    if p not in ok:
        raise CommandError(args, 'exited %d' % p)
    #end
#end

def RunCommandIO(args,infn,outfn):
    p = os.fork()
    if p == 0:
        os.dup2(os.open(infn,os.O_RDONLY),0)
        os.dup2(os.open(outfn,os.O_CREAT|os.O_TRUNC|os.O_WRONLY),1)
        os.execvp(args[0], args)
    else:
        s = os.waitpid(p,0)
        o = os.WEXITSTATUS(s[1])
        if not os.WIFEXITED(s[1]) or o != 0:
            raise CommandError(args, 'exited %d' % o)
        #end
    #end
#end

class TimedTest:
    def __init__(self, target, source, runnable,
                 skip_trials = SKIP_TRIALS,
                 min_trials = MIN_TRIALS,
                 max_trials = MAX_TRIALS,
                 min_stddev_pct = MIN_STDDEV_PCT):
        self.target = target
        self.source = source
        self.runnable = runnable

        self.skip_trials = skip_trials
        self.min_trials = min(min_trials, max_trials)
        self.max_trials = max_trials
        self.min_stddev_pct = min_stddev_pct

        self.encode_time = self.DoTest(DFILE,
                                       lambda x: x.Encode(self.target, 
                                                          self.source, DFILE))
        self.encode_size = runnable.EncodeSize(DFILE)

        self.decode_time = self.DoTest(RFILE,
                                       lambda x: x.Decode(DFILE, 
                                                          self.source, RFILE),
                                       )
        runnable.Verify(self.target, RFILE)
    #end

    def DoTest(self, fname, func):
        trials   = 0
        measured = []

        while 1:
            try:
                os.remove(fname)
            except OSError:
                pass

            start_time  = time.time()
            start_clock = time.clock()

            func(self.runnable)

            total_clock = (time.clock() - start_clock)
            total_time  = (time.time() - start_time)

            elap_time  = max(total_time,  0.0000001)
            elap_clock = max(total_clock, 0.0000001)

            trials = trials + 1

            # skip some of the first trials
            if trials > self.skip_trials:
                measured.append((elap_clock, elap_time))
                #print 'measurement total: %.1f ms' % (total_time * 1000.0)

            # at least so many
            if trials < (self.skip_trials + self.min_trials):
                #print 'continue: need more trials: %d' % trials
                continue

            # compute %variance
            done = 0
            if self.skip_trials + self.min_trials <= 2:
                measured = measured + measured;
                done = 1
            #end

            time_stat = StatList([x[1] for x in measured], 'elap time')
            sp = float(time_stat.s) / float(time_stat.mean)

            # what if MAX_TRIALS is exceeded?
            too_many = (trials - self.skip_trials) >= self.max_trials
            good = (100.0 * sp) < self.min_stddev_pct
            if done or too_many or good:
                trials = trials - self.skip_trials
                if not done and not good:
                    #print 'too many trials: %d' % trials
                    pass
                #clock = StatList([x[0] for x in measured], 'elap clock')
                return time_stat
            #end
        #end
    #end
#end

def Decimals(start, end):
    l = []
    step = start
    while 1:
        r = range(step, step * 10, step)
        l = l + r
        if step * 10 >= end:
            l.append(step * 10)
            break
        step = step * 10
    return l
#end

# This tests the raw speed of 0-byte inputs
def RunSpeedTest():
    for L in Decimals(MIN_RUN, MAX_RUN):
        SetFileSize(RUNFILE, L)

        trx = TimedTest(RUNFILE, None, Xdelta3Runner(['-W', str(1<<20)]))
        ReportSpeed(L, trx, '1MB ')

        trx = TimedTest(RUNFILE, None, Xdelta3Runner(['-W', str(1<<19)]))
        ReportSpeed(L, trx, '512k')

        trx = TimedTest(RUNFILE, None, Xdelta3Runner(['-W', str(1<<18)]))
        ReportSpeed(L, trx, '256k')

        trm = TimedTest(RUNFILE, None, Xdelta3Mod1(RUNFILE))
        ReportSpeed(L, trm, 'swig')

        trg = TimedTest(RUNFILE, None, GzipRun1())
        ReportSpeed(L,trg,'gzip')
    #end
#end

def SetFileSize(F,L):
    fd = os.open(F, os.O_CREAT | os.O_WRONLY)
    os.ftruncate(fd,L)
    assert os.fstat(fd).st_size == L
    os.close(fd)
#end

def ReportSpeed(L,tr,desc):
    print '%s run length %u: size %u: time %.3f ms: decode %.3f ms' % \
          (desc, L,
           tr.encode_size,
           tr.encode_time.mean * 1000.0,
           tr.decode_time.mean * 1000.0)
#end

class Xdelta3RunClass:
    def __init__(self, extra):
        self.extra = extra
    #end

    def __str__(self):
        return ' '.join(self.extra)
    #end

    def New(self):
        return Xdelta3Runner(self.extra)
    #end
#end

class Xdelta3Runner:
    # Use "forkexec" to get special command-line only features like
    # external compression support.
    def __init__(self, extra, forkexec=False):
        self.forkexec = forkexec
        self.extra = extra
    #end

    def Encode(self, target, source, output):
        args = (ALL_ARGS +
                self.extra +
                ['-e'])
        if source:
            args.append('-s')
            args.append(source)
        #end
        args = args + [target, output]
        self.Main(args)
    #end

    def Decode(self, input, source, output):
        args = (ALL_ARGS +
                ['-d'])
        if source:
            args.append('-s')
            args.append(source)
        #end
        args = args + [input, output]
        self.Main(args)
    #end

    def Verify(self, target, recon):
        if target[-3:] == ".gz":
            RunCommandIO(('gzip', '-dc'), target, CMPTMP1)
            RunCommandIO(('gzip', '-dc'), recon, CMPTMP2)
            RunCommand(('cmp', CMPTMP1, CMPTMP2))
        else:
            RunCommand(('cmp', target, recon))
    #end

    def EncodeSize(self, output):
        return os.stat(output).st_size
    #end

    def Main(self, args):
        try:
            if self.forkexec:
                RunCommand(['../xdelta3'] + args)
            else:
                xdelta3.xd3_main_cmdline(args)
        except Exception, e:
            raise CommandError(args, "xdelta3.main exception: %s" % e)
        #end
    #end
#end

class Xdelta3Mod1:
    def __init__(self, file):
        self.target_data = open(file, 'r').read()
    #end

    def Encode(self, ignore1, ignore2, ignore3):
        r1, encoded = xdelta3.xd3_encode_memory(self.target_data, None, 1000000, 1<<10)
        if r1 != 0:
            raise CommandError('memory', 'encode failed: %s' % r1)
        #end
        self.encoded = encoded
    #end

    def Decode(self, ignore1, ignore2, ignore3):
        r2, data1 = xdelta3.xd3_decode_memory(self.encoded, None, len(self.target_data))
        if r2 != 0:
            raise CommandError('memory', 'decode failed: %s' % r1)
        #end
        self.decoded = data1
    #end

    def Verify(self, ignore1, ignore2):
        if self.target_data != self.decoded:
            raise CommandError('memory', 'bad decode')
        #end
    #end

    def EncodeSize(self, ignore1):
        return len(self.encoded)
    #end
#end

class GzipRun1:
    def Encode(self, target, source, output):
        assert source == None
        RunCommandIO(['gzip', '-cf'], target, output)
    #end

    def Decode(self, input, source, output):
        assert source == None
        RunCommandIO(['gzip', '-dcf'], input, output)
    #end

    def Verify(self, target, recon):
        RunCommand(('cmp', target, recon))
    #end

    def EncodeSize(self, output):
        return os.stat(output).st_size
    #end
#end

class Xdelta1RunClass:
    def __str__(self):
        return 'xdelta1'
    #end

    def New(self):
        return Xdelta1Runner()
    #end
#end

class Xdelta1Runner:
    def Encode(self, target, source, output):
        assert source != None
        args = ['xdelta1', 'delta', '-q', source, target, output]
        RunCommand(args, [0, 1])
    #end

    def Decode(self, input, source, output):
        assert source != None
        args = ['xdelta1', 'patch', '-q', input, source, output]
        # Note: for dumb historical reasons, xdelta1 returns 1 or 0
        RunCommand(args)
    #end

    def Verify(self, target, recon):
        RunCommand(('cmp', target, recon))
    #end

    def EncodeSize(self, output):
        return os.stat(output).st_size
    #end
#end

# exceptions
class SkipRcsException:
    def __init__(self,reason):
        self.reason = reason
    #end
#end

class NotEnoughVersions:
    def __init__(self):
        pass
    #end
#end

class CommandError:
    def __init__(self,cmd,str):
        if type(cmd) is types.TupleType or \
           type(cmd) is types.ListType:
            cmd = reduce(lambda x,y: '%s %s' % (x,y),cmd)
        #end
        print 'command was: ',cmd
        print 'command failed: ',str
        print 'have fun debugging'
    #end
#end

class RcsVersion:
    def __init__(self,vstr):
        self.vstr = vstr
    #end
    def __cmp__(self,other):
        return cmp(self.date, other.date)
    #end
    def __str__(self):
        return str(self.vstr)
    #end
#end

class RcsFile:

    def __init__(self, fname):
        self.fname    = fname
        self.versions = []
        self.state    = HEAD_STATE
    #end

    def SetTotRev(self,s):
        self.totrev = int(s)
    #end

    def Rev(self,s):
        self.rev = RcsVersion(s)
        if len(self.versions) >= self.totrev:
            raise SkipRcsException('too many versions (in log messages)')
        #end
        self.versions.append(self.rev)
    #end

    def Date(self,s):
        self.rev.date = s
    #end

    def Match(self, line, state, rx, gp, newstate, f):
        if state == self.state:
            m = rx.match(line)
            if m:
                if f:
                    f(m.group(gp))
                #end
                self.state = newstate
                return 1
            #end
        #end
        return None
    #end

    def Sum1Rlog(self):
        f = os.popen('rlog '+self.fname, "r")
        l = f.readline()
        while l:
            if self.Match(l, HEAD_STATE, RE_TOTREV, 1, BAR_STATE, self.SetTotRev):
                pass
            elif self.Match(l, BAR_STATE, RE_BAR, 1, REV_STATE, None):
                pass
            elif self.Match(l, REV_STATE, RE_REV, 1, DATE_STATE, self.Rev):
                pass
            elif self.Match(l, DATE_STATE, RE_DATE, 1, BAR_STATE, self.Date):
                pass
            #end
            l = f.readline()
        #end
        c = f.close()
        if c != None:
            raise c
        #end
    #end

    def Sum1(self):
        st = os.stat(self.fname)
        self.rcssize = st.st_size
        self.Sum1Rlog()
        if self.totrev != len(self.versions):
            raise SkipRcsException('wrong version count')
        #end
        self.versions.sort()
    #end

    def Checkout(self,n):
        v      = self.versions[n]
        out    = open(self.Verf(n), "w")
        cmd    = 'co -ko -p%s %s' % (v.vstr, self.fname)
        total  = 0
        (inf,
         stream,
         err)  = os.popen3(cmd, "r")
        inf.close()
        buf    = stream.read()
        while buf:
            total = total + len(buf)
            out.write(buf)
            buf = stream.read()
        #end
        v.vsize = total
        estr = ''
        buf = err.read()
        while buf:
            estr = estr + buf
            buf = err.read()
        #end
        if stream.close():
            raise CommandError(cmd, 'checkout failed: %s\n%s\n%s' % (v.vstr, self.fname, estr))
        #end
        out.close()
        err.close()
    #end

    def Vdate(self,n):
        return self.versions[n].date
    #end

    def Vstr(self,n):
        return self.versions[n].vstr
    #end

    def Verf(self,n):
        return os.path.join(TMPDIR, 'input.%d' % n)
    #end

    def FilePairsByDate(self, runclass):
        if self.totrev < 2:
            raise NotEnoughVersions()
        #end
        self.Checkout(0)
        ntrials = []
        if self.totrev < 2:
            return vtrials
        #end
        for v in range(0,self.totrev-1):
            if v > 1:
                os.remove(self.Verf(v-1))
            #end
            self.Checkout(v+1)
            if os.stat(self.Verf(v)).st_size < MIN_SIZE or \
               os.stat(self.Verf(v+1)).st_size < MIN_SIZE:
                continue
            #end

            result = TimedTest(self.Verf(v+1),
                               self.Verf(v),
                               runclass.New())

            target_size = os.stat(self.Verf(v+1)).st_size

            ntrials.append(result)
        #end

        os.remove(self.Verf(self.totrev-1))
        os.remove(self.Verf(self.totrev-2))
        return ntrials
    #end

    def AppendVersion(self, f, n):
        self.Checkout(n)
        rf = open(self.Verf(n), "r")
        data = rf.read()
        f.write(data)
        rf.close()
        return len(data)
    #end

class RcsFinder:
    def __init__(self):
        self.subdirs  = []
        self.rcsfiles = []
        self.others   = []
        self.skipped  = []
        self.biground = 0
    #end

    def Scan1(self,dir):
        dents = os.listdir(dir)
        subdirs  = []
        rcsfiles = []
        others   = []
        for dent in dents:
            full = os.path.join(dir, dent)
            if os.path.isdir(full):
                subdirs.append(full)
            elif dent[len(dent)-2:] == ",v":
                rcsfiles.append(RcsFile(full))
            else:
                others.append(full)
            #end
        #end
        self.subdirs  = self.subdirs  + subdirs
        self.rcsfiles = self.rcsfiles + rcsfiles
        self.others   = self.others   + others
        return subdirs
    #end

    def Crawl(self, dir):
        subdirs = [dir]
        while subdirs:
            s1 = self.Scan1(subdirs[0])
            subdirs = subdirs[1:] + s1
        #end
    #end

    def Summarize(self):
        good = []
        for rf in self.rcsfiles:
            try:
                rf.Sum1()
                if rf.totrev < 2:
                    raise SkipRcsException('too few versions (< 2)')
                #end
            except SkipRcsException, e:
                #print 'skipping file %s: %s' % (rf.fname, e.reason)
                self.skipped.append(rf)
            else:
                good.append(rf)
            #end
        self.rcsfiles = good
    #end

    def AllPairsByDate(self, runclass):
        results = []
        good = []
        for rf in self.rcsfiles:
            try:
                results = results + rf.FilePairsByDate(runclass)
            except SkipRcsException:
                print 'file %s has compressed versions: skipping' % (rf.fname)
            except NotEnoughVersions:
                print 'testing %s on %s: not enough versions' % (runclass, rf.fname)
            else:
                good.append(rf)
            #end
        self.rcsfiles = good
        self.ReportPairs(runclass, results)
        return results
    #end

    def ReportPairs(self, name, results):
        encode_time = 0
        decode_time = 0
        encode_size = 0
        for r in results:
            encode_time += r.encode_time.mean
            decode_time += r.decode_time.mean
            encode_size += r.encode_size
        #end
        print '%s rcs: encode %.2f s: decode %.2f s: size %d' % \
              (name, encode_time, decode_time, encode_size)
    #end

    def MakeBigFiles(self, rand):
        f1 = open(TMPDIR + "/big.1", "w")
        f2 = open(TMPDIR + "/big.2", "w")
        population = []
        for file in self.rcsfiles:
            if len(file.versions) < 2:
                continue
            population.append(file)
        #end
        f1sz = 0
        f2sz = 0
        fcount = int(len(population) * FILE_P)
        assert fcount > 0
        for file in rand.sample(population, fcount):
            m = IGNORE_FILENAME.match(file.fname)
            if m != None:
                continue
            #end
            r1, r2 = rand.sample(xrange(0, len(file.versions)), 2)
            f1sz += file.AppendVersion(f1, r1)
            f2sz += file.AppendVersion(f2, r2)
            #m.update('%s,%s,%s ' % (file.fname[len(RCSDIR):], 
            #file.Vstr(r1), file.Vstr(r2)))
        #end
        testkey = 'rcs%d' % self.biground
        self.biground = self.biground + 1

        print '%s; source %u bytes; target %u bytes' % (testkey, f1sz, f2sz)
        f1.close()
        f2.close()
        return (TMPDIR + "/big.1",
                TMPDIR + "/big.2",
                testkey)
    #end

    def Generator(self):
        return lambda rand: self.MakeBigFiles(rand)
    #end
#end

# find a set of RCS files for testing
def GetTestRcsFiles():
    rcsf = RcsFinder()
    rcsf.Crawl(RCSDIR)
    if len(rcsf.rcsfiles) == 0:
        raise CommandError('', 'no RCS files')
    #end
    rcsf.Summarize()
    print "rcsfiles: rcsfiles %d; subdirs %d; others %d; skipped %d" % (
        len(rcsf.rcsfiles),
        len(rcsf.subdirs),
        len(rcsf.others),
        len(rcsf.skipped))
    print StatList([x.rcssize for x in rcsf.rcsfiles], "rcssize").str
    print StatList([x.totrev for x in rcsf.rcsfiles], "totrev").str
    return rcsf
#end

class SampleDataTest:
    def __init__(self, dirs):
        dirs_in = dirs
        self.pairs = []
        while dirs:
            d = dirs[0]
            dirs = dirs[1:]
            l = os.listdir(d)
            files = []
            for e in l:
                p = os.path.join(d, e)
                if os.path.isdir(p):
                    dirs.append(p)
                else:
                    files.append(p)
                #end
            #end
            if len(files) > 1:
                files.sort()
                for x in xrange(len(files)):
                    for y in xrange(len(files)):
                        self.pairs.append((files[x], files[y],
                                           '%s-%s' % (files[x], files[y])))
                    #end
                #end
            #end
        #end
        print "Sample data test using %d file pairs in %s" % (
            len(self.pairs), dirs_in)
    #end

    def Generator(self):
        return lambda rand: rand.choice(self.pairs)
    #end
#end

# configs are represented as a list of values,
# program takes a list of strings:
def ConfigToArgs(config):
    args = [ '-C',
             ','.join([str(x) for x in config[0:SOFT_CONFIG_CNT]])]
    for i in range(SOFT_CONFIG_CNT, len(CONFIG_ORDER)):
        key = CONFIG_ARGMAP[CONFIG_ORDER[i]]
        val = config[i]
        if val == 'true' or val == 'false':
            if val == 'true':
                args.append('%s' % key)
            #end
        else:
            args.append('%s=%s' % (key, val))
        #end
    #end
    return args
#end

#
class RandomTest:
    def __init__(self, tnum, tinput, config, syntuple = None):
        self.mytinput = tinput[2]
        self.myconfig = config
        self.tnum = tnum

        if syntuple != None:
            self.runtime = syntuple[0]
            self.compsize = syntuple[1]
            self.decodetime = None
        else:
            args = ConfigToArgs(config)
            result = TimedTest(tinput[1], tinput[0], Xdelta3Runner(args))

            self.runtime = result.encode_time.mean
            self.compsize = result.encode_size
            self.decodetime = result.decode_time.mean
        #end

        self.score = None
        self.time_pos = None
        self.size_pos = None
        self.score_pos = None
    #end

    def __str__(self):
        decodestr = ' %s' % self.decodetime
        return 'time %.6f%s size %d%s << %s >>%s' % (
            self.time(), ((self.time_pos != None) and 
                          (" (%s)" % self.time_pos) or ""),
            self.size(), ((self.size_pos != None) and 
                          (" (%s)" % self.size_pos) or ""),
            c2str(self.config()),
            decodestr)
    #end

    def time(self):
        return self.runtime
    #end

    def size(self):
        return self.compsize
    #end

    def config(self):
        return self.myconfig
    #end

    def score(self):
        return self.score
    #end

    def tinput(self):
        return self.mytinput
    #end
#end

def PosInAlist(l, e):
    for i in range(0, len(l)):
        if l[i][1] == e:
            return i;
        #end
    #end
    return -1
#end

# Generates a set of num_results test configurations, given the list of
# retest-configs.
def RandomTestConfigs(rand, input_configs, num_results):

    outputs = input_configs[:]
    have_set = dict([(c,c) for c in input_configs])

    # Compute a random configuration
    def RandomConfig():
        config = []
        cmap = {}
        for key in CONFIG_ORDER:
            val = cmap[key] = (INPUT_SPEC(rand)[key])(cmap)
            config.append(val)
        #end
        return tuple(config)
    #end

    while len(outputs) < num_results:
        newc = None
        for i in xrange(100):
            c = RandomConfig()
            if have_set.has_key(c):
                continue
            #end
            have_set[c] = c
            newc = c
            break
        if newc is None:
            print 'stopped looking for configs at %d' % len(outputs)
            break
        #end
        outputs.append(c)
    #end
    outputs.sort()
    return outputs
#end

def RunOptimizationLoop(rand, generator, rounds):
    configs = []
    for rnum in xrange(rounds):
        configs = RandomTestConfigs(rand, configs, MAX_RESULTS)
        tinput = generator(rand)
        tests = []
        for x in xrange(len(configs)):
            t = RandomTest(x, tinput, configs[x])
            print 'Round %d test %d: %s' % (rnum, x, t)
            tests.append(t)
        #end
        results = ScoreTests(tests)

        for r in results:
            c = r.config()
            if not test_all_config_results.has_key(c):
                test_all_config_results[c] = [r]
            else:
                test_all_config_results[c].append(r)
            #end
        #end

        #GraphResults('expt%d' % rnum, results)
        #GraphSummary('sum%d' % rnum, results)

        # re-test some fraction
        configs = [r.config() for r in results[0:int(MAX_RESULTS * KEEP_P)]]
    #end
#end

# TODO: cleanup
test_all_config_results = {}

def ScoreTests(results):
    scored = []
    timed = []
    sized = []

    t_min = float(min([test.time() for test in results]))
    #t_max = float(max([test.time() for test in results]))
    s_min = float(min([test.size() for test in results]))
    #s_max = float(max([test.size() for test in results]))

    for test in results:

        # Hyperbolic function. Smaller scores still better
        red = 0.999  # minimum factors for each dimension are 1/1000
        test.score = ((test.size() - s_min * red) *
                      (test.time() - t_min * red))

        scored.append((test.score, test))
        timed.append((test.time(), test))
        sized.append((test.size(), test))
    #end

    scored.sort()
    timed.sort()
    sized.sort()

    best_by_size = []
    best_by_time = []

    pos = 0
    for (score, test) in scored:
        pos += 1
        test.score_pos = pos
    #end

    scored = [x[1] for x in scored]

    for test in scored:
        test.size_pos = PosInAlist(sized, test)
        test.time_pos = PosInAlist(timed, test)
    #end

    for test in scored:
        c = test.config()
        s = 0.0
        print 'H-Score: %0.9f %s' % (test.score, test)
    #end

    return scored
#end

def GraphResults(desc, results):
    f = open("data-%s.csv" % desc, "w")
    for r in results:
        f.write("%0.9f\t%d\t# %s\n" % (r.time(), r.size(), r))
    #end
    f.close()
    os.system("./plot.sh data-%s.csv plot-%s.jpg" % (desc, desc))
#end

def GraphSummary(desc, results_ignore):
    test_population = 0
    config_ordered = []

    # drops duplicate test/config pairs (TODO: don't retest them)
    for config, cresults in test_all_config_results.items():
        input_config_map = {}
        uniq = []
        for test in cresults:
            assert test.config() == config
            test_population += 1
            key = test.tinput()
            if not input_config_map.has_key(key):
                input_config_map[key] = {}
            #end
            if input_config_map[key].has_key(config):
                print 'skipping repeat test %s vs. %s' % (input_config_map[key][config], test)
                continue
            #end
            input_config_map[key][config] = test
            uniq.append(test)
        #end
        config_ordered.append(uniq)
    #end

    # sort configs descending by number of tests
    config_ordered.sort(lambda x, y: len(y) - len(x))

    print 'population %d: %d configs %d results' % \
          (test_population,
           len(config_ordered),
           len(config_ordered[0]))

    if config_ordered[0] == 1:
        return
    #end

    # a map from test-key to test-list w/ various configs
    input_set = {}
    osize = len(config_ordered)

    for i in xrange(len(config_ordered)):
        config = config_ordered[i][0].config()
        config_tests = config_ordered[i]

        #print '%s has %d tested inputs' % (config, len(config_tests))

        if len(input_set) == 0:
            input_set = dict([(t.tinput(), [t]) for t in config_tests])
            continue
        #end

        # a map from test-key to test-list w/ various configs
        update_set = {}
        for r in config_tests:
            t = r.tinput()
            if input_set.has_key(t):
                update_set[t] = input_set[t] + [r]
            else:
                #print 'config %s does not have test %s' % (config, t)
                pass
            #end
        #end

        if len(update_set) <= 1:
            break
        #end

        input_set = update_set

        # continue if there are more w/ the same number of inputs
        if i < (len(config_ordered) - 1) and \
           len(config_ordered[i + 1]) == len(config_tests):
            continue
        #end

        # synthesize results for multi-test inputs
        config_num = None

        # map of config to sum(various test-keys)
        smap = {}
        for (key, tests) in input_set.items():
            if config_num == None:
                # config_num should be the same in all elements
                config_num = len(tests)
                smap = dict([(r.config(),
                              (r.time(),
                               r.size()))
                             for r in tests])
            else:
                # compuate the per-config sum of time/size
                assert config_num == len(tests)
                smap = dict([(r.config(),
                              (smap[r.config()][0] + r.time(),
                               smap[r.config()][1] + r.size()))
                             for r in tests])
            #end
        #end

        if config_num == 1:
            continue
        #end

        if len(input_set) == osize:
            break
        #end

        summary = '%s-%d' % (desc, len(input_set))
        osize = len(input_set)

        print 'generate %s w/ %d configs' % (summary, config_num)
        syn = [RandomTest(0, (None, None, summary), config,
                          syntuple = (smap[config][0], smap[config][1]))
               for config in smap.keys()]
        syn = ScoreTests(syn)
        #print 'smap is %s' % (smap,)
        #print 'syn is %s' % (' and '.join([str(x) for x in syn]))
        #GraphResults(summary, syn)
    #end
#end

def RunRegressionTest(pairs, rounds):
    for args in [
        [],
        ['-S=djw'],
        ['-B=412907520'],
        ['-B 412907520', ],

                 ]:
        print "Args %s" % (args)
        for (file1, file2, testkey) in pairs:
            ttest = TimedTest(file1, file2, Xdelta3Runner(args, forkexec=True),
                              skip_trials = 0,
                              min_trials = 1,
                              max_trials = 1)
            print "Source %s\nTarget %s\nEncode %s\nDecode %s\nSize %s\n\n" % (
                file1, file2,
                ttest.encode_time.str,
                ttest.decode_time.str,
                ttest.encode_size)
    #end
#end

if __name__ == "__main__":
    try:
        RunCommand(['rm', '-rf', TMPDIR])
        os.mkdir(TMPDIR)

        #rcsf = GetTestRcsFiles()
        #generator = rcsf.Generator()

        sample = SampleDataTest([SAMPLEDIR])
        generator = sample.Generator()

        rand = random.Random(135135135135135)

        RunRegressionTest(sample.pairs, TEST_ROUNDS)

        #RunSpeedTest()

        # the idea below is to add the default configurations and
        # xdelta1 to the optimization loop:
        #x3r = rcsf.AllPairsByDate(Xdelta3RunClass(['-1', '-3', '-6']))
        #x3r = rcsf.AllPairsByDate(Xdelta3RunClass(['-9']))
        #x3r = rcsf.AllPairsByDate(Xdelta3RunClass(['-9', '-S', 'djw']))
        #x3r = rcsf.AllPairsByDate(Xdelta3RunClass(['-1', '-S', 'djw']))
        #x3r = rcsf.AllPairsByDate(Xdelta3RunClass(['-9', '-T']))
        #x1r = rcsf.AllPairsByDate(Xdelta1RunClass())

    except CommandError:
        pass
    else:
        RunCommand(['rm', '-rf', TMPDIR])
        pass
    #end
#end
