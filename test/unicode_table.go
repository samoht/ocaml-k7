// Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
// SPDX-License-Identifier: ISC

// unicode_table writes lib/unicode.txt, the table of lib/unicode.ml: the
// code points of each table a \p class of Go's regexp/syntax names, one per
// line as its name and hexadecimal ranges, then the names Go's
// unicodeTable resolves to another table, one per line as name=table, with
// a caret before a table the name negates. Regenerate with
//
//	go run unicode_table.go > ../lib/unicode.txt
package main

import (
	"fmt"
	"sort"
	"strings"
	"unicode"
)

// canonicalName is regexp/syntax's.
func canonicalName(name string) string {
	var b []byte
	first := true
	for i := range len(name) {
		c := name[i]
		switch {
		case c == '_' || c == '-' || c == ' ':
			continue
		case first:
			if 'a' <= c && c <= 'z' {
				c -= 'a' - 'A'
			}
			first = false
		default:
			if 'A' <= c && c <= 'Z' {
				c += 'a' - 'A'
			}
		}
		b = append(b, c)
	}
	return string(b)
}

var special = map[string]*unicode.RangeTable{
	"Any":   {R16: []unicode.Range16{{Lo: 0, Hi: 1<<16 - 1, Stride: 1}}, R32: []unicode.Range32{{Lo: 1 << 16, Hi: unicode.MaxRune, Stride: 1}}},
	"Ascii": {R16: []unicode.Range16{{Lo: 0, Hi: 0x7f, Stride: 1}}},
}

func table(key string) *unicode.RangeTable {
	if t := special[key]; t != nil {
		return t
	}
	if t := unicode.Categories[key]; t != nil {
		return t
	}
	return unicode.Scripts[key]
}

// resolve is regexp/syntax's unicodeTable: the table a canonical name
// names and whether the name negates it.
func resolve(name string) (string, bool) {
	switch name {
	case "Any", "Ascii":
		return name, false
	case "Assigned":
		return "Cn", true
	case "Lc":
		return "LC", false
	}
	if unicode.Categories[name] != nil || unicode.Scripts[name] != nil {
		return name, false
	}
	for alias, actual := range unicode.CategoryAliases {
		if canonicalName(alias) == name {
			return actual, false
		}
	}
	for script := range unicode.Scripts {
		if canonicalName(script) == name {
			return script, false
		}
	}
	return "", false
}

func ranges(t *unicode.RangeTable) string {
	var b strings.Builder
	lo, hi := rune(-2), rune(-2)
	add := func(c rune) {
		if c == hi+1 {
			hi = c
			return
		}
		if lo >= 0 {
			flush(&b, lo, hi)
		}
		lo, hi = c, c
	}
	for _, r := range t.R16 {
		for c := rune(r.Lo); c <= rune(r.Hi); c += rune(r.Stride) {
			add(c)
		}
	}
	for _, r := range t.R32 {
		for c := rune(r.Lo); c <= rune(r.Hi); c += rune(r.Stride) {
			add(c)
		}
	}
	if lo >= 0 {
		flush(&b, lo, hi)
	}
	return b.String()
}

func flush(b *strings.Builder, lo, hi rune) {
	if lo == hi {
		fmt.Fprintf(b, " %x", lo)
	} else {
		fmt.Fprintf(b, " %x-%x", lo, hi)
	}
}

func main() {
	keys := []string{"Any", "Ascii"}
	for k := range unicode.Categories {
		keys = append(keys, k)
	}
	for k := range unicode.Scripts {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	names := map[string]bool{"Assigned": true}
	for _, k := range keys {
		fmt.Println(k + ranges(table(k)))
		names[canonicalName(k)] = true
	}
	for a := range unicode.CategoryAliases {
		names[canonicalName(a)] = true
	}
	var aliases []string
	for name := range names {
		key, negated := resolve(name)
		switch {
		case key == "":
			panic(name)
		case negated:
			aliases = append(aliases, name+"=^"+key)
		case key != name:
			aliases = append(aliases, name+"="+key)
		}
	}
	sort.Strings(aliases)
	for _, a := range aliases {
		fmt.Println(a)
	}
}
