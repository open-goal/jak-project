package main

import (
	"fmt"
	"io"
	"path"
	"os"
	"sort"
	"time"

	"xdelta"
)

const (
	xdataset = "/volume/home/jmacd/src/testdata"
	xcompare = "/volume/home/jmacd/src/xdelta-devel/xdelta3/build/x86_64-pc-linux-gnu-m64/xoff64/xdelta3"
	xdelta3  = "/volume/home/jmacd/src/xdelta-64bithash/xdelta3/build/x86_64-pc-linux-gnu-m64/usize64/xoff64/xdelta3"
	seed = 1422253499919909358
)

type Config struct {
	srcbuf_size int64
	window_size int64
	blocksize   int
}

func NewC() Config {
	// TODO make these (and above) flags
	return Config{1<<26, 1<<22, 1<<16}
}

func (c Config) smokeTest(t *xdelta.TestGroup, p xdelta.Program) {
	target := "Hello world!"
	source := "Hello world, nice to meet you!"

	enc, err := t.Exec("encode", p, true, []string{"-e"})
	if err != nil {
		t.Panic(err)
	}
	dec, err := t.Exec("decode", p, true, []string{"-d"})
	if err != nil {
		t.Panic(err)
	}

	encodeout := t.Drain(enc.Stdout, "encode.stdout")
	decodeout := t.Drain(dec.Stdout, "decode.stdout")

	t.Empty(enc.Stderr, "encode")
	t.Empty(dec.Stderr, "decode")

	t.TestWrite("encode.stdin", enc.Stdin, []byte(target))
	t.TestWrite("encode.srcin", enc.Srcin, []byte(source))

	t.TestWrite("decode.stdin", dec.Stdin, <-encodeout)
	t.TestWrite("decode.srcin", dec.Srcin, []byte(source))

	if do := string(<-decodeout); do != target {
		t.Panic(fmt.Errorf("It's not working! %s\n!=\n%s\n", do, target))
	}
	t.Wait(enc, dec)
}

type PairTest struct {
	// Input
	Config
	program xdelta.Program
	source, target string

	// Output
	TestOutput
}

type TestOutput struct {
	encoded int64
	encDuration time.Duration
	decDuration time.Duration
	encSysDuration time.Duration
	decSysDuration time.Duration
}

func (to *TestOutput) Add(a TestOutput) {
	to.encoded += a.encoded
	to.encDuration += a.encDuration
	to.decDuration += a.decDuration
	to.encSysDuration += a.encSysDuration
	to.decSysDuration += a.decSysDuration
}

func (to *TestOutput) String() string {
	return fmt.Sprintf("SIZE: %v\tT: %v\tTSYS: %v\tDT: %v\tDTSYS: %v",
		to.encoded, to.encDuration, to.encSysDuration, to.decDuration, to.encSysDuration)
}

// P is the test program, Q is the reference version.
func (cfg Config) datasetTest(t *xdelta.TestGroup, p, q xdelta.Program) {
	dir, err := os.Open(xdataset)
	if err != nil {
		t.Panic(err)
	}
	dents, err := dir.Readdir(-1)
	if err != nil {
		t.Panic(err)
	}
	paths := make([]string, len(dents))
	var total int64
	for i, d := range dents {
		if !d.Mode().IsRegular() {
			continue
		}
		paths[i] = fmt.Sprint(xdataset, "/", d.Name())
		total += d.Size()
	}
	meansize := total / int64(len(dents))
	largest  := uint(20)
	for ; largest <= 31 && 1<<largest < meansize; largest++ {}

	sort.Strings(paths)

	testSum := map[uint]*TestOutput{}
	compSum := map[uint]*TestOutput{}

	for _, in1 := range paths {
		for _, in2 := range paths {
			if in1 == in2 { continue }

			// 1/4, 1/2, and 1 of the power-of-2 rounded-up mean size
			for b := largest - 2; b <= largest; b++ {
				if _, has := testSum[b]; !has {
					testSum[b] = &TestOutput{}
					compSum[b] = &TestOutput{}
				}
				c1 := cfg
				c1.srcbuf_size = 1<<b
				ptest := &PairTest{c1, p, in1, in2, TestOutput{-1, 0, 0, 0, 0}}
				ptest.datasetPairTest(t, 1<<b);
				qtest := &PairTest{c1, q, in1, in2, TestOutput{-1, 0, 0, 0, 0}}
				qtest.datasetPairTest(t, 1<<b)

				testSum[b].Add(ptest.TestOutput)
				compSum[b].Add(qtest.TestOutput)

 				fmt.Printf("%s, %s: %.2f%% %+d/%d\n\tE:%.2f%%/%s(%.2f%%/%s) D:%.2f%%/%s(%.2f%%/%s) [B=%d]\n",
					path.Base(in1), path.Base(in2),
					float64(ptest.encoded - qtest.encoded) * 100.0 / float64(qtest.encoded),
					ptest.encoded - qtest.encoded,
					qtest.encoded,
					(ptest.encDuration - qtest.encDuration).Seconds() * 100.0 / qtest.encDuration.Seconds(),
					qtest.encDuration,
					(ptest.decDuration - qtest.decDuration).Seconds() * 100.0 / qtest.decDuration.Seconds(),
					qtest.encDuration,
					(ptest.encSysDuration - qtest.encSysDuration).Seconds() * 100.0 / qtest.encSysDuration.Seconds(),
					qtest.encSysDuration,
					(ptest.decSysDuration - qtest.decSysDuration).Seconds() * 100.0 / qtest.decSysDuration.Seconds(),
					qtest.decSysDuration,
					1<<b)
			}
		}
	}
	var keys []uint
	for k, _ := range testSum {
		keys = append(keys, k)
	}
	for _, k := range keys {		
		fmt.Printf("B=%v\nTEST: %v\nCOMP: %v\n", 1<<k, testSum[k], compSum[k])
	}
}

func (pt *PairTest) datasetPairTest(t *xdelta.TestGroup, meanSize int64) {
	cfg := pt.Config
	eargs := []string{"-e", fmt.Sprint("-B", cfg.srcbuf_size), // "-q",
		fmt.Sprint("-W", cfg.window_size), "-s", pt.source,
		"-I0", "-S", "none", pt.target}
	enc, err := t.Exec("encode", pt.program, false, eargs)
	if err != nil {
		t.Panic(err)
	}

	dargs := []string{"-dc", fmt.Sprint("-B", cfg.srcbuf_size), //"-q",
		fmt.Sprint("-W", cfg.window_size), "-s", pt.source,
		"-S", "none"}

	dec, err := t.Exec("decode", pt.program, false, dargs)
	if err != nil {
		t.Panic(err)
	}
	tgt_check, err := os.Open(pt.target)
	if err != nil {
		t.Panic(err)
	}
	tgt_info, err := tgt_check.Stat()
	if err != nil {
		t.Panic(err)
	}
	t.Empty(enc.Stderr, "encode")
	t.Empty(dec.Stderr, "decode")
	t.CopyStreams(enc.Stdout, dec.Stdin, &pt.encoded)
	t.CompareStreams(dec.Stdout, tgt_check, tgt_info.Size())

	t.Wait(enc, dec)

	pt.decDuration = dec.Cmd.ProcessState.UserTime()
	pt.encDuration = enc.Cmd.ProcessState.UserTime()
	pt.decSysDuration = dec.Cmd.ProcessState.SystemTime()
	pt.encSysDuration = enc.Cmd.ProcessState.SystemTime()
}

func (cfg Config) offsetTest(t *xdelta.TestGroup, p xdelta.Program, offset, length int64) {
	eargs := []string{"-e", "-0", fmt.Sprint("-B", cfg.srcbuf_size), "-q",
		fmt.Sprint("-W", cfg.window_size)}
	enc, err := t.Exec("encode", p, true, eargs)
	if err != nil {
		t.Panic(err)
	}
	
	dargs := []string{"-d", fmt.Sprint("-B", cfg.srcbuf_size), "-q",
		fmt.Sprint("-W", cfg.window_size)}
	dec, err := t.Exec("decode", p, true, dargs)
	if err != nil {
		t.Panic(err)
	}

	// The pipe used to read the decoder output and compare
	// against the target.
	read, write := io.Pipe()

	t.Empty(enc.Stderr, "encode")
	t.Empty(dec.Stderr, "decode")

	var encoded_size int64
	t.CopyStreams(enc.Stdout, dec.Stdin, &encoded_size)
	t.CompareStreams(dec.Stdout, read, length)

	// The decoder output ("read", above) is compared with the
	// test-provided output ("write", below).  The following
	// generates two identical inputs.
	t.WriteRstreams("encode", seed, offset, length, enc.Srcin, enc.Stdin)
	t.WriteRstreams("decode", seed, offset, length, dec.Srcin, write)
	t.Wait(enc, dec)

	expect := cfg.srcbuf_size - offset
	if float64(encoded_size) < (0.95 * float64(expect)) ||
		float64(encoded_size) > (1.05 * float64(expect)) {
		t.Fail("encoded size should be ~=", expect, ", actual ", encoded_size)
	}
}

func main() {
	r, err := xdelta.NewRunner()
	if err != nil {
		panic(err)
	}
	defer r.Cleanup()

	cfg := NewC()

	prog := xdelta.Program{xdelta3}

	r.RunTest("smoketest", func(t *xdelta.TestGroup) { cfg.smokeTest(t, prog) })

	for i := uint(29); i <= 33; i += 1 {
		// The arguments to offsetTest are offset, source
		// window size, and file size. The source window size
		// is (2 << i) and (in the 3.0x release branch) is
		// limited to 2^31, so the the greatest value of i is
		// 30.
		cfg.srcbuf_size = 2 << i
		r.RunTest(fmt.Sprint("offset", i), func(t *xdelta.TestGroup) {
			cfg.offsetTest(t, prog, 1 << i, 3 << i) })
	}
	
	comp := xdelta.Program{xcompare}

	r.RunTest("dataset", func(t *xdelta.TestGroup) { cfg.datasetTest(t, prog, comp) })
}
