// Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
// SPDX-License-Identifier: ISC

// match_oracle writes match_cases.txt: random patterns and texts, each
// with whether Go's regexp MatchString, which VHS's Wait runs, holds.
// Regenerate with
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

var toks = []string{"(?i)", "(?i:", "(?-i)", ")", "(", "|", "\\b", "\\B", "^", "$", "(?m)", "(?s)", ".", "*", "+", "?", "{2}", "{1,2}", "a", "A", "s", "S", "k", "K", "e", "é", "É", "ſ", "K", "ß", "ẞ", "σ", "Σ", "ς", "ǅ", "ǆ", "Ǆ", "\\w", "\\W", "\\d", "\\s", "[a-z]", "[^a-z]", "[[:alpha:]]", "[[:^alpha:]]", "[\\W]", "[^\\w]", "x", "_", "1", " ", "\\x{212a}", "[é-ë]", "[A-Z]", "\\p{Greek}", "\\pL", "\\PL", "\\p{Lu}", "[\\p{^Lu}]", "\\p{Latin}", "[\\P{Ll}]", "\\p{Assigned}", "\\p{Ascii}", "\\pN", "\\p{Common}", "\\p{Any}", "\\P{Lt}"}

var texts = []string{"a", "A", "s", "S", "k", "K", "e", "é", "É", "ſ", "K", "ß", "ẞ", "σ", "Σ", "ς", "ǅ", "ǆ", "Ǆ", "x", "_", "1", " ", "\n", "-", "中", "́", "α", "Ω", "\u0378", "٣", "𐌀"}

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
