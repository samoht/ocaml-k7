// Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
// SPDX-License-Identifier: ISC

// tape_oracle writes tape_cases.txt: random tape lines built from VHS
// fragments, each with 1 if `vhs validate` (VHS's lexer and parser) accepts
// the line on its own and 0 otherwise. Regenerate with
//
//	go run tape_oracle.go 3000 > tape_cases.txt
package main

import (
	"fmt"
	"math/rand"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
)

var heads = []string{"Type", "Type@10ms", "Sleep", "Wait", "Wait+Screen", "Wait+Line", "Wait@1s", "Wait+Screen@0ms", "Set", "Output", "Require", "Env", "Source", "Screenshot", "Copy", "Paste", "Ctrl", "Alt", "Shift", "Enter", "Tab", "Backspace@5ms", "Up", "PageDown", "ScrollUp", "Hide", "Show", "End", "Home", "#", "ms", "Escape", "Insert", "Delete", "Space"}

var toks = []string{"\"a\"", "'b c'", "`d`", "\"x.gif\"", "x.gif", "out/", "a.tape", "s.png", "\"Line\"", "Line", "Screen", "/re/", "/[/", "/a\\/b/", "/a\\\\/", "{\"k\": 1}", "{", "1", "0", "10", "1.5", "1.2.3", ".5", "ms", "s", "m", "@", "@10ms", "@0", "+", "+A", "+Alt", "+Shift", "+c", "+Enter", "+[", "+-", "+ab", "%", "-1", "-", "^", "\\", "=", "[", "]", "#c", "true", "false", "FontSize", "Width", "Margin", "MarginFill", "#ff00ff", "#zz", "WindowBar", "Colorful", "Rings", "Bogus", "CursorBlink", "TypingSpeed", "WaitTimeout", "WaitPattern", "LoopOffset", "Theme", "Shell", "bash", "Enter", "Type", "Hide", "\"", "'", ":", "é", "99999999999999999999", "9223372036", "A", "x"}

func main() {
	n, _ := strconv.Atoi(os.Args[1])
	r := rand.New(rand.NewSource(int64(n)))
	dir, err := os.MkdirTemp("", "tape-oracle")
	if err != nil {
		panic(err)
	}
	defer os.RemoveAll(dir)
	// A Source line reads its tape, so give the one in toks a body.
	if err := os.WriteFile(filepath.Join(dir, "a.tape"), []byte("Enter\n"), 0o644); err != nil {
		panic(err)
	}
	lines := make([]string, n)
	files := make([]string, n)
	for i := range lines {
		parts := []string{heads[r.Intn(len(heads))]}
		for j := r.Intn(4); j > 0; j-- {
			sep := " "
			if r.Intn(4) == 0 {
				sep = ""
			}
			parts = append(parts, sep+toks[r.Intn(len(toks))])
		}
		lines[i] = strings.Join(parts, "")
		files[i] = fmt.Sprintf("case-%05d.tape", i)
		if err := os.WriteFile(filepath.Join(dir, files[i]), []byte(lines[i]+"\n"), 0o644); err != nil {
			panic(err)
		}
	}
	// One run for all lines: vhs validate names each file it rejects.
	cmd := exec.Command("vhs", append([]string{"validate"}, files...)...)
	cmd.Dir = dir
	out, _ := cmd.CombinedOutput()
	for i, line := range lines {
		v := 1
		if strings.Contains(string(out), files[i]) {
			v = 0
		}
		fmt.Printf("%d\t%s\n", v, line)
	}
}
