package xdelta

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"sync/atomic"

	"golang.org/x/sys/unix"
)

var (
	tmpDir = "/tmp"
	srcSeq int64
)

func (t *TestGroup) Drain(f io.ReadCloser, desc string) <-chan []byte {
	c := make(chan []byte)
	t.Go(desc, func(g *Goroutine) {
		if b, err := ioutil.ReadAll(f); err != nil {
			g.Panic(err)
		} else {
			c <- b
		}
		g.OK()
	})
	return c
}

func (t *TestGroup) Empty(f io.ReadCloser, desc string) *Goroutine {
	return t.Go("empty:"+desc, func (g *Goroutine) {
		s := bufio.NewScanner(f)
		for s.Scan() {
			os.Stderr.Write([]byte(fmt.Sprint(desc, ": ", s.Text(), "\n")))
		}
		err := s.Err()
		f.Close()
		if err != nil {
			g.Panic(err)
		}
		g.OK()
	})
}

func (t *TestGroup) TestWrite(what string, f io.WriteCloser, b []byte) *Goroutine {
	return t.Go("write", func(g *Goroutine) {
		if _, err := f.Write(b); err != nil {
			g.Panic(err)
		}
		if err := f.Close(); err != nil {
			g.Panic(err)
		}
		g.OK()
	})
}

func (t *TestGroup) CopyStreams(r io.ReadCloser, w io.WriteCloser, written *int64) *Goroutine {
	return t.Go("copy", func(g *Goroutine) {
		nwrite, err := io.Copy(w, r)
		if err != nil {
			g.Panic(err)
		}
		err = r.Close()
		if err != nil {
			g.Panic(err)
		}
		err = w.Close()
		if err != nil {
			g.Panic(err)
		}
		g.OK()
		*written = nwrite
	})
}

func (t *TestGroup) CompareStreams(r1 io.ReadCloser, r2 io.ReadCloser, length int64) *Goroutine {
	return t.Go("compare", func(g *Goroutine) {
		b1 := make([]byte, blocksize)
		b2 := make([]byte, blocksize)
		var idx int64
		for length > 0 {
			c := blocksize
			if length < blocksize {
				c = int(length)
			}
			if _, err := io.ReadFull(r1, b1[0:c]); err != nil {
				g.Panic(err)
			}
			if _, err := io.ReadFull(r2, b2[0:c]); err != nil {
				g.Panic(err)
			}
			if bytes.Compare(b1[0:c], b2[0:c]) != 0 {
				fmt.Println("B1 is", string(b1[0:c]))
				fmt.Println("B2 is", string(b2[0:c]))			
				g.Panic(errors.New(fmt.Sprint("Bytes do not compare at ", idx)))
			}
			length -= int64(c)
			idx += int64(c)
		}
		g.OK()
	})
}

func (t *TestGroup) Exec(desc string, p Program, srcfifo bool, flags []string) (*Run, error) {
	var err error
	run := &Run{}
	args := []string{p.Path}
	if srcfifo {
		num := atomic.AddInt64(&srcSeq, 1)
		run.Srcfile = path.Join(t.Runner.Testdir, fmt.Sprint("source", num))
		if err = unix.Mkfifo(run.Srcfile, 0600); err != nil {
			return nil, err
		}
		read, write := io.Pipe()
		t.writeFifo(run.Srcfile, read)
		run.Srcin = write
		args = append(args, "-s")
		args = append(args, run.Srcfile)
	}
	if run.Stdin, err = run.Cmd.StdinPipe(); err != nil {
		return nil, err
	}
	if run.Stdout, err = run.Cmd.StdoutPipe(); err != nil {
		return nil, err
	}
	if run.Stderr, err = run.Cmd.StderrPipe(); err != nil {
		return nil, err
	}

	run.Cmd.Path = p.Path
	run.Cmd.Args = append(args, flags...)
	run.Cmd.Dir = t.Runner.Testdir
	if serr := run.Cmd.Start(); serr != nil {
		return nil, serr
	}
	return run, nil
}

func (t *TestGroup) Fail(v ...interface{}) {
	panic(fmt.Sprintln(v...))
}

func (t *TestGroup) writeFifo(srcfile string, read io.Reader) *Goroutine {
	return t.Go("compare", func(g *Goroutine) {
		fifo, err := os.OpenFile(srcfile, os.O_WRONLY, 0600)
		if err != nil {
			fifo.Close()
			g.Panic(err)
		}
		if _, err := io.Copy(fifo, read); err != nil {
			fifo.Close()
			g.Panic(err)
		}
		if err := fifo.Close(); err != nil {
			g.Panic(err)
		}
		g.OK()
	})
}
