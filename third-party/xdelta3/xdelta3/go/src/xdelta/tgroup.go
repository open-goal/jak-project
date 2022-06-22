package xdelta

import (
	"fmt"
	"runtime"
	"sync"
)

type TestGroup struct {
	*Runner
	main *Goroutine
	sync.Mutex
	sync.WaitGroup
	running []*Goroutine
	errors []error
	nonerrors []error  // For tolerated / expected conditions
}

type Goroutine struct {
	*TestGroup
	name string
	done bool
}

func (g *Goroutine) String() string {
	return fmt.Sprint("[", g.name, "]")
}

func (g *Goroutine) finish(err error) {
	wait := false
	tg := g.TestGroup
	sbuf := make([]byte, 4096)
	sbuf = sbuf[0:runtime.Stack(sbuf, false)]
	if err != nil {
		err = fmt.Errorf("%v:%v:%v", g.name, err, string(sbuf))
	}
	tg.Lock()
	if g.done {
		if err != nil {
			tg.nonerrors = append(tg.nonerrors, err)
		}
	} else {
		wait = true
		g.done = true
		if err != nil {
			tg.errors = append(tg.errors, err)
		}
	}
	tg.Unlock()
	if wait {
		tg.WaitGroup.Done()
	}
}

func (g *Goroutine) OK() {
	g.finish(nil)
}

func (g *Goroutine) Panic(err error) {
	g.finish(err)
	if g != g.TestGroup.main {
		runtime.Goexit()
	}
}

func (t *TestGroup) Main() *Goroutine { return t.main }

func (t *TestGroup) Panic(err error) { t.Main().Panic(err) }

func (t *TestGroup) Go(name string, f func(*Goroutine)) *Goroutine {
	g := &Goroutine{t, name, false}
	t.Lock()
	t.WaitGroup.Add(1)
	t.running = append(t.running, g)
	t.Unlock()
	go f(g)
	return g
}

func (t *TestGroup) Wait(procs... *Run) {
	t.Main().OK()
	t.WaitGroup.Wait()
	for _, p := range procs {
		if err := p.Wait(); err != nil {
			t.errors = append(t.errors, err)
		}
	}
	for _, err := range t.errors {
		fmt.Println(":ERROR:", err)
	}
	for _, err := range t.nonerrors {
		fmt.Println("(ERROR)", err)
	}
	if len(t.errors) != 0 {
		t.Fail("Test failed with", len(t.errors), "errors")
	}
}
