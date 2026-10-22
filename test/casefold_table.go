// Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
// SPDX-License-Identifier: ISC

// casefold_table writes lib/casefold.txt: the orbits of Go's
// unicode.SimpleFold. Regenerate with
//
//	go run casefold_table.go > ../lib/casefold.txt
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	seen := map[rune]bool{}
	for r := rune(0); r <= unicode.MaxRune; r++ {
		if seen[r] || unicode.SimpleFold(r) == r {
			continue
		}
		orbit := []string{}
		for f := r; ; {
			seen[f] = true
			orbit = append(orbit, fmt.Sprintf("%x", f))
			f = unicode.SimpleFold(f)
			if f == r {
				break
			}
		}
		fmt.Println(strings.Join(orbit, " "))
	}
}
