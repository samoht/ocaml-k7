// Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
// SPDX-License-Identifier: ISC

// regexp_oracle writes regexp_cases.txt: random patterns, each with whether
// Go's regexp.Compile accepts it. Regenerate with
//
//	go run regexp_oracle.go 3000 > regexp_cases.txt
package main

import (
	"fmt"
	"math/rand"
	"os"
	"regexp"
	"strconv"
)

var toks = []string{"a", "b", "Z", "0", "1", "2", "7", "9", ",", "(", ")", "(?", "(?:", "(?i)", "(?i-s:", "(?-", "(?P<", "(?<", "n>", ">", "<", "P", "[", "]", "[^", "^", "$", ".", "-", "|", "*", "+", "?", "{", "}", "{2}", "{1,3}", "{,", "{0}", "{1000}", "{1001}", "\\", "\\\\", "\\d", "\\D", "\\w", "\\s", "\\b", "\\B", "\\A", "\\z", "\\Z", "\\C", "\\Q", "\\E", "\\x", "\\x4", "\\x41", "\\x{", "\\x{10FFFF}", "\\x{110000}", "\\0", "\\1", "\\12", "\\8", "\\p", "\\pL", "\\pX", "\\p{", "\\p{Greek}", "\\P{^greek}", "\\p{old_italic}", "\\p{Foo}", "[:alpha:]", "[:^digit:]", "[:foo:]", ":]", "/", "\\/", "é", "\xff", "\\t", "\\n", "\\a", "\\v", "\\f", "\\r", "\\e", "\\-", "\\]", "\\[", "_", " ", "i", "m", "s", "U", "x", ":", "=", "!", "#", "{01}", "{99999999999}", "z-a", "a-z", "\\d-z"}

func main() {
	n, _ := strconv.Atoi(os.Args[1])
	r := rand.New(rand.NewSource(int64(n)))
	for i := 0; i < n; i++ {
		k := 1 + r.Intn(10)
		s := ""
		for j := 0; j < k; j++ {
			s += toks[r.Intn(len(toks))]
		}
		v := 0
		if _, err := regexp.Compile(s); err == nil {
			v = 1
		}
		fmt.Printf("%d\t%s\n", v, s)
	}
}
