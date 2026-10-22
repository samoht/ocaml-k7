// Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
// SPDX-License-Identifier: ISC

// theme_oracle writes theme_cases.txt: random JSON themes, each with
// whether VHS's json.Unmarshal accepts it, and its background and red.
// Regenerate with
//
//	go run theme_oracle.go 2000 > theme_cases.txt
package main

import (
	"encoding/json"
	"fmt"
	"math/rand"
	"os"
	"strconv"
)

// Theme is VHS's, from themes.go at v0.12.1.
type Theme struct {
	Name          string `json:"name"`
	Background    string `json:"background"`
	Foreground    string `json:"foreground"`
	Selection     string `json:"selection"`
	Cursor        string `json:"cursor"`
	CursorAccent  string `json:"cursorAccent"`
	Black         string `json:"black"`
	BrightBlack   string `json:"brightBlack"`
	Red           string `json:"red"`
	BrightRed     string `json:"brightRed"`
	Green         string `json:"green"`
	BrightGreen   string `json:"brightGreen"`
	Yellow        string `json:"yellow"`
	BrightYellow  string `json:"brightYellow"`
	Blue          string `json:"blue"`
	BrightBlue    string `json:"brightBlue"`
	Magenta       string `json:"magenta"`
	BrightMagenta string `json:"brightMagenta"`
	Cyan          string `json:"cyan"`
	BrightCyan    string `json:"brightCyan"`
	White         string `json:"white"`
	BrightWhite   string `json:"brightWhite"`
}

var toks = []string{"{", "}", "[", "]", ":", ",", " ", "\"background\"", "\"BACKGROUND\"", "\"bacKground\"", "\"r\u017fd\"", "\"red\"", "\"Red\"", "\"\u212aey\"", "\"other\"", "\"#123\"", "\"#abcdef\"", "\"rgb(1, 2, 3)\"", "\"\"", "\"\\u0041\"", "\"\\ud800\"", "\"\\ud83d\\ude00\"", "\"\\x\"", "\"\\n\"", "\"\xff\"", "null", "true", "fals", "1", "-0.5e3", "01", "-", "1.", "\"a", "\"é\""}

func main() {
	n, _ := strconv.Atoi(os.Args[1])
	r := rand.New(rand.NewSource(int64(n)))
	for i := 0; i < n; i++ {
		var s string
		if r.Intn(2) == 0 {
			s = "{"
			k := r.Intn(4)
			for j := 0; j < k; j++ {
				if j > 0 {
					s += ","
				}
				s += toks[7+r.Intn(8)] + ":" + toks[15+r.Intn(16)]
			}
			s += "}"
		} else {
			s = "{"
			k := 1 + r.Intn(8)
			for j := 0; j < k; j++ {
				s += toks[r.Intn(len(toks))]
			}
		}
		var t Theme
		if err := json.Unmarshal([]byte(s), &t); err == nil {
			fmt.Printf("1\t%x\t%x\t%s\n", t.Background, t.Red, s)
		} else {
			fmt.Printf("0\t\t\t%s\n", s)
		}
	}
}
