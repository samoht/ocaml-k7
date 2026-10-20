// Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
// SPDX-License-Identifier: ISC

// match_oracle writes match_cases.txt: random patterns, built from RE2
// fragments that stress case folding and word boundaries, and random texts,
// each with 1 if Go's regexp MatchString (what VHS's Wait runs) holds and 0
// otherwise, the pattern and the text in hexadecimal. Patterns Go does not
// compile are left out. Regenerate with
//
//	go run match_oracle.go 4000 > match_cases.txt
package main

import (
	"fmt"
	"math/rand"
	"os"
	"regexp"
	"strconv"
)

var toks = []string{"(?i)", "(?i:", "(?-i)", ")", "(", "|", "\\b", "\\B", "^", "$", "(?m)", "(?s)", ".", "*", "+", "?", "{2}", "{1,2}", "a", "A", "s", "S", "k", "K", "e", "é", "É", "ſ", "K", "ß", "ẞ", "σ", "Σ", "ς", "ǅ", "ǆ", "Ǆ", "\\w", "\\W", "\\d", "\\s", "[a-z]", "[^a-z]", "[[:alpha:]]", "[[:^alpha:]]", "[\\W]", "[^\\w]", "x", "_", "1", " ", "\\x{212a}", "[é-ë]", "[A-Z]"}

var texts = []string{"a", "A", "s", "S", "k", "K", "e", "é", "É", "ſ", "K", "ß", "ẞ", "σ", "Σ", "ς", "ǅ", "ǆ", "Ǆ", "x", "_", "1", " ", "\n", "-", "中", "́"}

func main() {
	n, _ := strconv.Atoi(os.Args[1])
	r := rand.New(rand.NewSource(int64(n)))
	for i := 0; i < n; {
		p := ""
		for j := 1 + r.Intn(6); j > 0; j-- {
			p += toks[r.Intn(len(toks))]
		}
		re, err := regexp.Compile(p)
		if err != nil {
			continue
		}
		t := ""
		for j := r.Intn(6); j > 0; j-- {
			t += texts[r.Intn(len(texts))]
		}
		v := 0
		if re.MatchString(t) {
			v = 1
		}
		fmt.Printf("%d\t%x\t%x\n", v, p, t)
		i++
	}
}
