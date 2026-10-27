The demo of the README, which k7 records natively with
dune exec -- k7 play --output doc/demo.svg doc/demo.t; dune runs it as a
cram test too.

  $ echo "Recorded without a browser"
  Recorded without a browser
  $ printf '\033[1;32mbold green\033[0m, \033[34mblue\033[0m, \033[7minverse\033[0m\n'
  bold green, blue, inverse
  $ printf 'wide 中文, combining e\314\201, emoji \360\237\221\215\n'
  wide 中文, combining é, emoji 👍
  $ echo $((6 * 7))
  42
