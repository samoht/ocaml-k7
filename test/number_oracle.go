// Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
// SPDX-License-Identifier: ISC

// number_oracle writes number_cases.txt: random values built from number
// fragments, each with 1 or 0 for whether Go's strconv.Atoi and then
// strconv.ParseFloat (what VHS runs on an integer and a float setting)
// accept it. Regenerate with
//
//	go run number_oracle.go 3000 > number_cases.txt
package main

import (
	"fmt"
	"math/rand"
	"os"
	"strconv"
)

var toks = []string{"+", "-", "0", "1", "7", "9", "12", "007", "_", ".", "e", "E", "p", "P", "x", "X", "0x", "0X", "0b", "0o", "a", "f", "F", "g", "i", "n", "inf", "Inf", "INFINITY", "infinity", "infin", "nan", "NaN", "e-", "e+", "e1", "p-", "p2", "1e308", "1e309", "1e-400", "9223372036854775807", "9223372036854775808", "-9223372036854775808", "1_000", " ", "0x1p1023", "0x1p1024", "0x1.fffffffffffffp1023", "0x1.fffffffffffff8p1023", "1.7976931348623157e308", "1.7976931348623159e308", "٣", " "}

func main() {
	n, _ := strconv.Atoi(os.Args[1])
	r := rand.New(rand.NewSource(int64(n)))
	cases := append([]string{""}, toks...)
	for i := 0; i < n; i++ {
		k := 1 + r.Intn(5)
		s := ""
		for j := 0; j < k; j++ {
			s += toks[r.Intn(len(toks))]
		}
		cases = append(cases, s)
	}
	for _, s := range cases {
		a, f := 0, 0
		if _, err := strconv.Atoi(s); err == nil {
			a = 1
		}
		if _, err := strconv.ParseFloat(s, 64); err == nil {
			f = 1
		}
		fmt.Printf("%d%d\t%s\n", a, f, s)
	}
}
