(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  background : int;
  foreground : int;
  cursor : int;
  cursor_accent : int;
  ansi : int array;
}

(* VHS's [DefaultTheme] in style.go and themes.go. *)
let default =
  {
    background = 0x171717;
    foreground = 0xdddddd;
    cursor = 0xdddddd;
    cursor_accent = 0x171717;
    ansi =
      [| 0x282a2e; 0xd74e6f; 0x31bb71; 0xd3e561; 0x8056ff; 0xed61d7; 0x04d7d7;
         0xbfbfbf; 0x4d4d4d; 0xfe5f86; 0x00d787; 0xebff71; 0x9b79ff; 0xff7aea;
         0x00fefe; 0xe6e6e6 |];
  }
[@@ocamlformat "disable"]

(* The colours of themes.json of VHS v0.12.1 (MIT, Charm), in its order:
   background, foreground, cursor (xterm.js's white where the theme has
   none), then the sixteen ANSI colours. *)
let themes =
  [
    ("3024 Day",
     "f7f7f7 4a4543 4a4543 090300 db2d20 01a252 fded02 01a0e4 a16a94 b5e4f4 a5a2a2 5c5855 e8bbd0 3a3432 4a4543 807d7c d6d5d4 cdab53 f7f7f7");
    ("3024 Night",
     "090300 a5a2a2 a5a2a2 090300 db2d20 01a252 fded02 01a0e4 a16a94 b5e4f4 a5a2a2 5c5855 e8bbd0 3a3432 4a4543 807d7c d6d5d4 cdab53 f7f7f7");
    ("Aardvark Blue",
     "102040 dddddd 007acc 191919 aa342e 4b8c0f dbba00 1370d3 c43ac3 008eb0 bebebe 454545 f05b50 95dc55 ffe763 60a4ec e26be2 60b6cb f7f7f7");
    ("Abernathy",
     "111416 eeeeec bbbbbb 000000 cd0000 00cd00 cdcd00 1093f5 cd00cd 00cdcd faebd7 404040 ff0000 00ff00 ffff00 11b5f6 ff00ff 00ffff ffffff");
    ("Adventure",
     "040404 feffff feffff 040404 d84a33 5da602 eebb6e 417ab3 e5c499 bdcfe5 dbded8 685656 d76b42 99b52c ffb670 97d7ef aa7900 bdcfe5 e4d5c7");
    ("AdventureTime",
     "1f1d45 f8dcc0 efbf38 050404 bd0013 4ab118 e7741e 0f4ac6 665993 70a598 f8dcc0 4e7cbf fc5f5a 9eff6e efc11a 1997c6 9b5953 c8faf4 f6f5fb");
    ("Afterglow",
     "212121 d0d0d0 d0d0d0 151515 ac4142 7e8e50 e5b567 6c99bb 9f4e85 7dd6cf d0d0d0 505050 ac4142 7e8e50 e5b567 6c99bb 9f4e85 7dd6cf f5f5f5");
    ("Alabaster",
     "f7f7f7 000000 007acc 000000 aa3731 448c27 cb9000 325cc0 7a3e9d 0083b2 f7f7f7 777777 f05050 60cb00 ffbc5d 007acc e64ce6 00aacb f7f7f7");
    ("AlienBlood",
     "0f1610 637d75 73fa91 112616 7f2b27 2f7e25 717f24 2f6a7f 47587f 327f77 647d75 3c4812 e08009 18e000 bde000 00aae0 0058e0 00e0c4 73fa91");
    ("Andromeda",
     "262a33 e5e5e5 f8f8f0 000000 cd3131 05bc79 e5e512 2472c8 bc3fbc 0fa8cd e5e5e5 666666 cd3131 05bc79 e5e512 2472c8 bc3fbc 0fa8cd e5e5e5");
    ("Apple Classic",
     "2c2b2b d5a200 c7c7c7 000000 c91b00 00c200 c7c400 0225c7 ca30c7 00c5c7 c7c7c7 686868 ff6e67 5ffa68 fffc67 6871ff ff77ff 60fdff ffffff");
    ("arcoiris",
     "201f1e eee4d9 7a1c1c 333333 da2700 12c258 ffc656 518bfc e37bd9 63fad5 bab2b2 777777 ffb9b9 e3f6aa ffddaa b3e8f3 cbbaf9 bcffc7 efefef");
    ("Argonaut",
     "0e1019 fffaf4 ff0018 232323 ff000f 8ce10b ffb900 008df8 6d43a6 00d8eb ffffff 444444 ff2740 abe15b ffd242 0092ff 9a5feb 67fff0 ffffff");
    ("Arthur",
     "1c1c1c ddeedd e2bbef 3d352a cd5c5c 86af80 e8ae5b 6495ed deb887 b0c4de bbaa99 554444 cc5533 88aa22 ffa75d 87ceeb 996600 b0c4de ddccbb");
    ("AtelierSulphurpool",
     "202746 979db4 979db4 202746 c94922 ac9739 c08b30 3d8fd1 6679cc 22a2c9 979db4 6b7394 c76b29 293256 5e6687 898ea4 dfe2f1 9c637a f5f7ff");
    ("Atom",
     "161719 c5c8c6 d0d0d0 000000 fd5ff1 87c38a ffd7b1 85befd b9b6fc 85befd e0e0e0 000000 fd5ff1 94fa36 f5ffa8 96cbfe b9b6fc 85befd e0e0e0");
    ("AtomOneLight",
     "f9f9f9 2a2c33 bbbbbb 000000 de3e35 3f953a d2b67c 2f5af3 950095 3f953a bbbbbb 000000 de3e35 3f953a d2b67c 2f5af3 a00095 3f953a ffffff");
    ("Aurora",
     "23262e ffca28 ee5d43 23262e f0266f 8fd46d ffe66d 0321d7 ee5d43 03d6b8 c74ded 292e38 f92672 8fd46d ffe66d 03d6b8 ee5d43 03d6b8 c74ded");
    ("ayu",
     "0f1419 e6e1cf f29718 000000 ff3333 b8cc52 e7c547 36a3d9 f07178 95e6cb ffffff 323232 ff6565 eafe84 fff779 68d5ff ffa3aa c7fffd ffffff");
    ("Ayu Mirage",
     "1f2430 cbccc6 ffcc66 191e2a ed8274 a6cc70 fad07b 6dcbfa cfbafa 90e1c6 c7c7c7 686868 f28779 bae67e ffd580 73d0ff d4bfff 95e6cb ffffff");
    ("ayu_light",
     "fafafa 5c6773 ff6a00 000000 ff3333 86b300 f29718 41a6d9 f07178 4dbf99 ffffff 323232 ff6565 b8e532 ffc94a 73d8ff ffa3aa 7ff1cb ffffff");
    ("Banana Blueberry",
     "191323 cccccc e07d13 17141f ff6b7f 00bd9c e6c62f 22e8df dc396a 56b6c2 f1f1f1 495162 fe9ea1 98c379 f9e46b 91fff4 da70d6 bcf3ff ffffff");
    ("Batman",
     "1b1d1e 6f6f6f fcef0c 1b1d1e e6dc44 c8be46 f4fd22 737174 747271 62605f c6c5bf 505354 fff78e fff27d feed6c 919495 9a9a9d a3a3a6 dadbd6");
    ("Belafonte Day",
     "d5ccba 45373c 45373c 20111b be100e 858162 eaa549 426a79 97522c 989a9c 968c83 5e5252 be100e 858162 eaa549 426a79 97522c 989a9c d5ccba");
    ("Belafonte Night",
     "20111b 968c83 968c83 20111b be100e 858162 eaa549 426a79 97522c 989a9c 968c83 5e5252 be100e 858162 eaa549 426a79 97522c 989a9c d5ccba");
    ("BirdsOfParadise",
     "2a1f1d e0dbb7 573d26 573d26 be2d26 6ba18a e99d2a 5a86ad ac80a6 74a6ad e0dbb7 9b6c4a e84627 95d8ba d0d150 b8d3ed d19ecb 93cfd7 fff9d5");
    ("Blazer",
     "0d1926 d9e6f2 d9e6f2 000000 b87a7a 7ab87a b8b87a 7a7ab8 b87ab8 7ab8b8 d9d9d9 262626 dbbdbd bddbbd dbdbbd bdbddb dbbddb bddbdb ffffff");
    ("Blue Matrix",
     "101116 00a2ff 76ff9f 101116 ff5680 00ff9c fffc58 00b0ff d57bff 76c1ff c7c7c7 686868 ff6e67 5ffa68 fffc67 6871ff d682ec 60fdff ffffff");
    ("BlueBerryPie",
     "1c0c28 babab9 fcfad6 0a4c62 99246e 5cb1b3 eab9a8 90a5bd 9d54a7 7e83cc f0e8d6 201637 c87272 0a6c7e 7a3188 39173d bc94b7 5e6071 0a6c7e");
    ("BlueDolphin",
     "006984 c5f2ff ffcc00 292d3e ff8288 b4e88d f4d69f 82aaff e9c1ff 89ebff d0d0d0 434758 ff8b92 ddffa7 ffe585 9cc4ff ddb0f6 a3f7ff ffffff");
    ("BlulocoDark",
     "282c34 b9c0cb ffcc00 41444d fc2f52 25a45c ff936a 3476ff 7a82da 4483aa cdd4e0 8f9aae ff6480 3fc56b f9c859 10b1fe ff78f8 5fb9bc ffffff");
    ("BlulocoLight",
     "f9f9f9 373a41 f32759 373a41 d52753 23974a df631c 275fe4 823ff1 27618d babbc2 676a77 ff6480 3cbc66 c5a332 0099e1 ce33c0 6d93bb d3d3d3");
    ("Borland",
     "0000a4 ffff4e ffa560 4f4f4f ff6c60 a8ff60 ffffb6 96cbfe ff73fd c6c5fe eeeeee 7c7c7c ffb6b0 ceffac ffffcc b5dcff ff9cfe dfdffe ffffff");
    ("Breeze",
     "31363b eff0f1 eff0f1 31363b ed1515 11d116 f67400 1d99f3 9b59b6 1abc9c eff0f1 7f8c8d c0392b 1cdc9a fdbc4b 3daee9 8e44ad 16a085 fcfcfc");
    ("Bright Lights",
     "191919 b3c9d7 f34b00 191919 ff355b b7e876 ffc251 76d4ff ba76e7 6cbfb5 c2c8d7 191919 ff355b b7e876 ffc251 76d5ff ba76e7 6cbfb5 c2c8d7");
    ("Broadcast",
     "2b2b2b e6e1dc ffffff 000000 da4939 519f50 ffd24a 6d9cbe d0d0ff 6e9cbe ffffff 323232 ff7b6b 83d182 ffff7c 9fcef0 ffffff a0cef0 ffffff");
    ("Brogrammer",
     "131313 d6dbe5 b9b9b9 1f1f1f f81118 2dc55e ecba0f 2a84d2 4e5ab7 1081d6 d6dbe5 d6dbe5 de352e 1dd361 f3bd09 1081d6 5350b9 0f7ddb ffffff");
    ("Bubbles",
     "161d38 ffffff bbbbbb 000000 f9555f 21b089 fef02a 589df6 944d95 1f9ee7 bbbbbb 555555 fa8c8f 35bb9a ffff55 589df6 e75699 3979bc ffffff");
    ("Builtin Dark",
     "000000 bbbbbb bbbbbb 000000 bb0000 00bb00 bbbb00 0000bb bb00bb 00bbbb bbbbbb 555555 ff5555 55ff55 ffff55 5555ff ff55ff 55ffff ffffff");
    ("Builtin Light",
     "ffffff 000000 000000 000000 bb0000 00bb00 bbbb00 0000bb bb00bb 00bbbb bbbbbb 555555 ff5555 55ff55 ffff55 5555ff ff55ff 55ffff ffffff");
    ("Builtin Pastel Dark",
     "000000 bbbbbb ffa560 4f4f4f ff6c60 a8ff60 ffffb6 96cbfe ff73fd c6c5fe eeeeee 7c7c7c ffb6b0 ceffac ffffcc b5dcff ff9cfe dfdffe ffffff");
    ("Builtin Solarized Dark",
     "002b36 839496 839496 073642 dc322f 859900 b58900 268bd2 d33682 2aa198 eee8d5 002b36 cb4b16 586e75 657b83 839496 6c71c4 93a1a1 fdf6e3");
    ("Builtin Solarized Light",
     "fdf6e3 657b83 657b83 073642 dc322f 859900 b58900 268bd2 d33682 2aa198 eee8d5 002b36 cb4b16 586e75 657b83 839496 6c71c4 93a1a1 fdf6e3");
    ("Builtin Tango Dark",
     "000000 ffffff ffffff 000000 cc0000 4e9a06 c4a000 3465a4 75507b 06989a d3d7cf 555753 ef2929 8ae234 fce94f 729fcf ad7fa8 34e2e2 eeeeec");
    ("Builtin Tango Light",
     "ffffff 000000 000000 000000 cc0000 4e9a06 c4a000 3465a4 75507b 06989a d3d7cf 555753 ef2929 8ae234 fce94f 729fcf ad7fa8 34e2e2 eeeeec");
    ("C64",
     "40318d 7869c4 7869c4 090300 883932 55a049 bfce72 40318d 8b3f96 67b6bd ffffff 000000 883932 55a049 bfce72 40318d 8b3f96 67b6bd f7f7f7");
    ("Calamity",
     "2f2833 d5ced9 d5ced9 2f2833 fc644d a5f69c e9d7a5 3b79c7 f92672 74d3de d5ced9 7e6c88 fc644d a5f69c e9d7a5 3b79c7 f92672 74d3de ffffff");
    ("Catppuccin Frappe",
     "303446 c6d0f5 f2d5cf 51576d e78284 a6d189 e5c890 8caaee f4b8e4 81c8be b5bfe2 626880 e78284 a6d189 e5c890 8caaee f4b8e4 81c8be a5adce");
    ("Catppuccin Latte",
     "eff1f5 4c4f69 dc8a78 5c5f77 d20f39 40a02b df8e1d 1e66f5 ea76cb 179299 acb0be 6c6f85 d20f39 40a02b df8e1d 1e66f5 ea76cb 179299 bcc0cc");
    ("Catppuccin Macchiato",
     "24273a cad3f5 f4dbd6 494d64 ed8796 a6da95 eed49f 8aadf4 f5bde6 8bd5ca b8c0e0 5b6078 ed8796 a6da95 eed49f 8aadf4 f5bde6 8bd5ca b8c0e0");
    ("Catppuccin Mocha",
     "1e1e2e cdd6f4 f5e0dc 45475a f38ba8 a6e3a1 f9e2af 89b4fa f5c2e7 94e2d5 bac2de 585b70 f38ba8 a6e3a1 f9e2af 89b4fa f5c2e7 94e2d5 a6adc8");
    ("catppuccin-frappe",
     "303446 c6d0f5 f2d5cf 51576d e78284 a6d189 e5c890 8caaee f4b8e4 81c8be b5bfe2 626880 e78284 a6d189 e5c890 8caaee f4b8e4 81c8be a5adce");
    ("catppuccin-latte",
     "eff1f5 4c4f69 dc8a78 5c5f77 d20f39 40a02b df8e1d 1e66f5 ea76cb 179299 acb0be 6c6f85 d20f39 40a02b df8e1d 1e66f5 ea76cb 179299 bcc0cc");
    ("catppuccin-macchiato",
     "24273a cad3f5 f4dbd6 494d64 ed8796 a6da95 eed49f 8aadf4 f5bde6 8bd5ca b8c0e0 5b6078 ed8796 a6da95 eed49f 8aadf4 f5bde6 8bd5ca a5adcb");
    ("catppuccin-mocha",
     "1e1e2e cdd6f4 f5e0dc 45475a f38ba8 a6e3a1 f9e2af 89b4fa f5c2e7 94e2d5 bac2de 585b70 f38ba8 a6e3a1 f9e2af 89b4fa f5c2e7 94e2d5 a6adc8");
    ("CGA",
     "000000 aaaaaa b8b8b8 000000 aa0000 00aa00 aa5500 0000aa aa00aa 00aaaa aaaaaa 555555 ff5555 55ff55 ffff55 5555ff ff55ff 55ffff ffffff");
    ("Chalk",
     "2b2d2e d2d8d9 708284 7d8b8f b23a52 789b6a b9ac4a 2a7fac bd4f5a 44a799 d2d8d9 888888 f24840 80c470 ffeb62 4196ff fc5275 53cdbd d2d8d9");
    ("Chalkboard",
     "29262f d9e6f2 d9e6f2 000000 c37372 72c373 c2c372 7372c3 c372c2 72c2c3 d9d9d9 323232 dbaaaa aadbaa dadbaa aaaadb dbaada aadadb ffffff");
    ("ChallengerDeep",
     "1e1c31 cbe1e7 fbfcfc 141228 ff5458 62d196 ffb378 65b2ff 906cff 63f2f1 a6b3cc 565575 ff8080 95ffa4 ffe9aa 91ddff c991e1 aaffe4 cbe3e7");
    ("Chester",
     "2c3643 ffffff b4b1b1 080200 fa5e5b 16c98d ffc83f 288ad6 d34590 28ddde e7e7e7 6f6b68 fa5e5b 16c98d feef6d 278ad6 d34590 27dede ffffff");
    ("Ciapre",
     "191c27 aea47a 92805b 181818 810009 48513b cc8b3f 576d8c 724d7c 5c4f4b aea47f 555555 ac3835 a6a75d dcdf7c 3097c6 d33061 f3dbb2 f4f4f4");
    ("CLRS",
     "ffffff 262626 6fd3fc 000000 f8282a 328a5d fa701d 135cd0 9f00bd 33c3c1 b3b3b3 555753 fb0416 2cc631 fdd727 1670ff e900b0 3ad5ce eeeeec");
    ("Cobalt Neon",
     "142838 8ff586 c4206f 142631 ff2320 3ba5ff e9e75c 8ff586 781aa0 8ff586 ba46b2 fff688 d4312e 8ff586 e9f06d 3c7dd2 8230a7 6cbc67 8ff586");
    ("Cobalt2",
     "132738 ffffff f0cc09 000000 ff0000 38de21 ffe50a 1460d2 ff005d 00bbbb bbbbbb 555555 f40e17 3bd01d edc809 5555ff ff55ff 6ae3fa ffffff");
    ("coffee_theme",
     "f5deb3 000000 c7c7c7 000000 c91b00 00c200 c7c400 0225c7 ca30c7 00c5c7 c7c7c7 686868 ff6e67 5ffa68 fffc67 6871ff ff77ff 60fdff ffffff");
    ("Contrast Light",
     "ffffff 051840 000000 2e1742 bf0000 77b01b 857e39 0d6bad 8700ff 41c2d0 e6e6e6 462366 ff0000 99c252 a39b46 008ccc af87ff 46d1e0 ffffff");
    ("coolnight",
     "010c18 ecdef4 38ff9d 0b3b61 ff3a3a 52ffd0 fff383 1376f9 c792ea ff5ed4 16fda2 63686d ff54b0 74ffd8 fcf5ae 388eff ae81ff ff6ad7 60fbbf");
    ("CrayonPonyFish",
     "150707 68525a 68525a 2b1b1d 91002b 579524 ab311b 8c87b0 692f50 e8a866 68525a 3d2b2e c5255d 8dff57 c8381d cfc9ff fc6cba ffceaf b0949d");
    ("Crystal Violet",
     "1e1423 737da0 2db9e6 0a0a14 e63232 32cd87 eba041 5050e6 6e14eb 2db9e6 d2d2d2 363b54 f7768e 41a6b5 e0af68 7aa2f7 bb9af7 7dcfff acb0d0");
    ("Cyber Cube",
     "161c2b a4b1cd c5d1eb 141d2b ff3e3e 9fef00 ffaf00 2e6cff 9f00ff 2de2b2 d5e0ff 767676 ff8484 c5f467 ffcc5c 5cb2ff cf8dfb 5cecc6 d5daff");
    ("Cyberdyne",
     "151144 00ff92 00ff9c 080808 ff8373 00c172 d2a700 0071cf ff90fe 6bffdd f1f1f1 2e2e2e ffc4be d6fcba fffed5 c2e3ff ffb2fe e6e7fe ffffff");
    ("cyberpunk",
     "332a57 e5e5e5 21f6bc 000000 ff7092 00fbac fffa6a 00bfff df95ff 86cbfe ffffff 000000 ff8aa4 21f6bc fff787 1bccfd e6aefe 99d6fc ffffff");
    ("CyberPunk2077",
     "272932 e455ae fdf500 272932 710000 1ac5b0 fdf500 9381ff 742d8b 00d0db d1c5c0 7b8097 c71515 40ffe9 fff955 37ebf3 cb1dcd 37ebf3 c1deff");
    ("Dark Pastel",
     "000000 ffffff bbbbbb 000000 ff5555 55ff55 ffff55 5555ff ff55ff 55ffff bbbbbb 555555 ff5555 55ff55 ffff55 5555ff ff55ff 55ffff ffffff");
    ("Dark+",
     "1e1e1e cccccc ffffff 000000 cd3131 0dbc79 e5e510 2472c8 bc3fbc 11a8cd e5e5e5 666666 f14c4c 23d18b f5f543 3b8eea d670d6 29b8db e5e5e5");
    ("darkermatrix",
     "070c0e 28380d 373a26 091013 002e18 6fa64c 595900 00cb6b 412a4d 125459 002e19 333333 00381d 90d762 e2e500 00ff87 412a4d 176c73 00381e");
    ("darkmatrix",
     "070c0e 3e5715 9fa86e 091013 006536 6fa64c 7e8000 2c9a84 452d53 114d53 006536 333333 00733d 90d762 e2e500 46d8b8 4a3059 12545a 006536");
    ("Darkside",
     "222324 bababa bbbbbb 000000 e8341c 68c256 f2d42c 1c98e8 8e69c9 1c98e8 bababa 000000 e05a4f 77b869 efd64b 387cd3 957bbe 3d97e2 bababa");
    ("deep",
     "090909 cdcdcd d0d0d0 000000 d70005 1cd915 d9bd26 5665ff b052da 50d2da e0e0e0 535353 fb0007 22ff18 fedc2b 9fa9ff e09aff 8df9ff ffffff");
    ("Desert",
     "333333 ffffff 00ff00 4d4d4d ff2b2b 98fb98 f0e68c cd853f ffdead ffa0a0 f5deb3 555555 ff5555 55ff55 ffff55 87ceff ff55ff ffd700 ffffff");
    ("DimmedMonokai",
     "1f1f1f b9bcba f83e19 3a3d43 be3f48 879a3b c5a635 4f76a1 855c8d 578fa4 b9bcba 888987 fb001f 0f722f c47033 186de3 fb0067 2e706d fdffb9");
    ("Django",
     "0b2f20 f8f8f8 336442 000000 fd6209 41a83e ffe862 245032 f8f8f8 9df39f ffffff 323232 ff943b 73da70 ffff94 568264 ffffff cfffd1 ffffff");
    ("DjangoRebornAgain",
     "051f14 dadedc ffcc00 000000 fd6209 41a83e ffe862 245032 f8f8f8 9df39f ffffff 323232 ff943b 73da70 ffff94 568264 ffffff cfffd1 ffffff");
    ("DjangoSmooth",
     "245032 f8f8f8 336442 000000 fd6209 41a83e ffe862 989898 f8f8f8 9df39f e8e8e7 323232 ff943b 73da70 ffff94 cacaca ffffff cfffd1 ffffff");
    ("Doom Peacock",
     "2b2a27 ede0ce 9c9c9d 1c1f24 cb4b16 26a6a6 bcd42a 2a6cc6 a9a1e1 5699af ede0ce 2b2a27 ff5d38 98be65 e6f972 51afef c678dd 46d9ff dfdfdf");
    ("DoomOne",
     "282c34 bbc2cf 51afef 000000 ff6c6b 98be65 ecbe7b a9a1e1 c678dd 51afef bbc2cf 000000 ff6655 99bb66 ecbe7b a9a1e1 c678dd 51afef bfbfbf");
    ("DotGov",
     "262c35 ebebeb d9002f 191919 bf091d 3d9751 f6bb34 17b2e0 7830b0 8bd2ed ffffff 191919 bf091d 3d9751 f6bb34 17b2e0 7830b0 8bd2ed ffffff");
    ("Dracula",
     "1e1f29 f8f8f2 bbbbbb 000000 ff5555 50fa7b f1fa8c bd93f9 ff79c6 8be9fd bbbbbb 555555 ff5555 50fa7b f1fa8c bd93f9 ff79c6 8be9fd ffffff");
    ("Dracula+",
     "212121 f8f8f2 eceff4 21222c ff5555 50fa7b ffcb6b 82aaff c792ea 8be9fd f8f8f2 545454 ff6e6e 69ff94 ffcb6b d6acff ff92df a4ffff f8f8f2");
    ("DraculaPlus",
     "212121 f8f8f2 ffffff 21222c ff5555 50fa7b ffcb6b 82aaff c792ea 8be9fd f8f8f2 545454 ff6e6e 69ff94 ffcb6b d6acff ff92df a4ffff f8f8f2");
    ("duckbones",
     "0e101a ebefc0 edf2c2 0e101a e03600 5dcd97 e39500 00a3cb 795ccc 00a3cb ebefc0 2b2f46 ff4821 58db9e f6a100 00b4e0 b3a1e6 00b4e0 b3b692");
    ("Duotone Dark",
     "1f1d27 b7a1ff ff9839 1f1d27 d9393e 2dcd73 d9b76e ffc284 de8d40 2488ff b7a1ff 353147 d9393e 2dcd73 d9b76e ffc284 de8d40 2488ff eae5ff");
    ("Earthsong",
     "292520 e5c7a9 f6f7ec 121418 c94234 85c54c f5ae2e 1398b9 d0633d 509552 e5c6aa 675f54 ff645a 98e036 e0d561 5fdaff ff9269 84f088 f6f7ec");
    ("Elemental",
     "22211d 807a74 facb80 3c3c30 98290f 479a43 7f7111 497f7d 7f4e2f 387f58 807974 555445 e0502a 61e070 d69927 79d9d9 cd7c54 59d599 fff1e9");
    ("Elementary",
     "181818 efefef bbbbbb 242424 d71c15 5aa513 fdb40c 063b8c e40038 2595e1 efefef 4b4b4b fc1c18 6bc219 fec80e 0955ff fb0050 3ea8fc 8c00ec");
    ("ENCOM",
     "000000 00a595 bbbbbb 000000 9f0000 008b00 ffd000 0081ff bc00ca 008b8b bbbbbb 555555 ff0000 00ee00 ffff00 0000ff ff00ff 00cdcd ffffff");
    ("Espresso",
     "323232 ffffff d6d6d6 353535 d25252 a5c261 ffc66d 6c99bb d197d9 bed6ff eeeeec 535353 f00c0c c2e075 e1e48b 8ab7d9 efb5f7 dcf4ff ffffff");
    ("Espresso Libre",
     "2a211c b8a898 ffffff 000000 cc0000 1a921c f0e53a 0066ff c5656b 06989a d3d7cf 555753 ef2929 9aff87 fffb5c 43a8ed ff818a 34e2e2 eeeeec");
    ("Everblush",
     "141b1e dadada dadada 232a2d e57474 8ccf7e e5c76b 67b0e8 c47fd5 6cbfbf b3b9b8 2d3437 ef7e7e 96d988 f4d67a 71baf2 ce89df 67cbe7 bdc3c2");
    ("Fahrenheit",
     "000000 ffffce bbbbbb 1d1d1d cda074 9e744d fecf75 720102 734c4d 979797 ffffce 000000 fecea0 cc734d fd9f4d cb4a05 4e739f fed04d ffffff");
    ("Fairyfloss",
     "5a5475 f8f8f2 f8f8f0 040303 f92672 c2ffdf e6c000 c2ffdf ffb8d1 c5a3ff f8f8f0 6090cb ff857f c2ffdf ffea00 c2ffdf ffb8d1 c5a3ff f8f8f0");
    ("Fideloper",
     "292f33 dbdae0 d4605a 292f33 cb1e2d edb8ac b7ab9b 2e78c2 c0236f 309186 eae3ce 092028 d4605a d4605a a86671 7c85c4 5c5db2 819090 fcf4df");
    ("FirefoxDev",
     "0e1011 7c8fa4 708284 002831 e63853 5eb83c a57706 359ddf d75cff 4b73a2 dcdcdc 001e27 e1003f 1d9000 cd9409 006fc0 a200da 005794 e2e2e2");
    ("Firewatch",
     "1e2027 9ba2b2 f6f7ec 585f6d d95360 5ab977 dfb563 4d89c4 d55119 44a8b6 e6e5ff 585f6d d95360 5ab977 dfb563 4c89c5 d55119 44a8b6 e6e5ff");
    ("FishTank",
     "232537 ecf0fe fecd5e 03073c c6004a acf157 fecd5e 525fb8 986f82 968763 ecf0fc 6c5b30 da4b8a dbffa9 fee6a9 b2befa fda5cd a5bd86 f6ffec");
    ("Flat",
     "002240 2cc55d e5be0c 222d3f a82320 32a548 e58d11 3167ac 781aa0 2c9370 b0b6ba 212c3c d4312e 2d9440 e5be0c 3c7dd2 8230a7 35b387 e7eced");
    ("Flatland",
     "1d1f21 b8dbef 708284 1d1d19 f18339 9fd364 f4ef6d 5096be 695abc d63865 ffffff 1d1d19 d22a24 a7d42c ff8949 61b9d0 695abc d63865 ffffff");
    ("Floraverse",
     "0e0d15 dbd1b9 bbbbbb 08002e 64002c 5d731a cd751c 1d6da1 b7077e 42a38c f3e0b8 331e4d d02063 b4ce59 fac357 40a4cf f12aae 62caa8 fff5db");
    ("ForestBlue",
     "051519 e2d8cd 9e9ecb 333333 f8818e 92d3a2 1a8e63 8ed0ce 5e468c 31658c e2d8cd 3d3d3d fb3d66 6bb48d 30c85a 39a7a2 7e62b3 6096bf e2d8cd");
    ("Framer",
     "111111 777777 fcdc08 141414 ff5555 98ec65 ffcc33 00aaff aa88ff 88ddff cccccc 414141 ff8888 b6f292 ffd966 33bbff cebbff bbecff ffffff");
    ("FrontEndDelight",
     "1b1c1d adadad cdcdcd 242526 f8511b 565747 fa771d 2c70b7 f02e4f 3ca1a6 adadad 5fac6d f74319 74ec4c fdc325 3393ca e75e4f 4fbce6 8c735b");
    ("FunForrest",
     "251200 dec165 e5591c 000000 d6262b 919c00 be8a13 4699a3 8d4331 da8213 ddc265 7f6a55 e55a1c bfc65a ffcb1b 7cc9cf d26349 e6a96b ffeaa3");
    ("Galaxy",
     "1d2837 ffffff bbbbbb 000000 f9555f 21b089 fef02a 589df6 944d95 1f9ee7 bbbbbb 555555 fa8c8f 35bb9a ffff55 589df6 e75699 3979bc ffffff");
    ("Galizur",
     "071317 ddeeff ddeeff 223344 aa1122 33aa11 ccaa22 2255cc 7755aa 22bbdd 8899aa 556677 ff1133 33ff11 ffdd33 3377ff aa77ff 33ddff bbccdd");
    ("Ganyu",
     "221b2f 5055c2 ac5487 12061b e06c75 c7e18d f8b433 5256c3 ddb2ff 2aadb3 ecebea 322a51 a5313d b4f42c f1d661 9aa8dd 9681ed 7ce7d8 f3f3f3");
    ("Github",
     "f4f4f4 3e3e3e 3f3f3f 3e3e3e 970b16 07962a f8eec7 003e8a e94691 89d1ec ffffff 666666 de0000 87d5a2 f1d007 2e6cba ffa29f 1cfafe ffffff");
    ("GitHub Dark",
     "101216 8b949e c9d1d9 000000 f78166 56d364 e3b341 6ca4f8 db61a2 2b7489 ffffff 4d4d4d f78166 56d364 e3b341 6ca4f8 db61a2 2b7489 ffffff");
    ("Glacier",
     "0c1115 ffffff 6c6c6c 2e343c bd0f2f 35a770 fb9435 1f5872 bd2523 778397 ffffff 404a55 bd0f2f 49e998 fddf6e 2a8bc1 ea4727 a0b6d3 ffffff");
    ("Glorious",
     "201d27 c8c8c9 ffffff 1e1e1e be0f17 868715 cc881a 16777a a04b73 578e57 978771 7f7061 f73028 d5d5d7 f7b125 4e917c c77089 7db669 e6d4a3");
    ("Grape",
     "171423 9f9fa1 a288f7 2d283f ed2261 1fa91b 8ddc20 487df4 8d35c9 3bdeed 9e9ea0 59516a f0729a 53aa5e b2dc87 a9bcec ad81c2 9de3eb a288f7");
    ("Grass",
     "13773d fff0a5 8c2800 000000 bb0000 00bb00 e7b000 0000a3 950062 00bbbb bbbbbb 555555 bb0000 00bb00 e7b000 0000bb ff55ff 55ffff ffffff");
    ("Grey-green",
     "002a1a ffffff fff400 000000 fe1414 74ff00 f1ff01 00deff ff00f0 00ffbc ffffff 666666 ff3939 00ff44 ffd100 00afff ff008a 00ffd3 f5ecec");
    ("Gruvbox Light",
     "fbf1c7 282828 282828 fbf1c7 9d0006 79740e b57614 076678 8f3f71 427b58 3c3836 9d8374 cc241d 98971a d79921 458588 b16186 689d69 7c6f64");
    ("GruvboxDark",
     "282828 ebdbb2 ebdbb2 282828 cc241d 98971a d79921 458588 b16286 689d6a a89984 928374 fb4934 b8bb26 fabd2f 83a598 d3869b 8ec07c ebdbb2");
    ("GruvboxDarkHard",
     "1b1b1b ebdbb2 ebdbb2 1b1b1b cc241d 98971a d79921 458588 b16286 689d6a a89984 928374 fb4934 b8bb26 fabd2f 83a598 d3869b 8ec07c ebdbb2");
    ("Guezwhoz",
     "1c1c1c d0d0d0 eeeeee 080808 ff5f5f 87d7af d7d787 5fafd7 afafff 5fd7d7 dadada 8a8a8a d75f5f afd7af d7d7af 87afd7 afafd7 87d7d7 dadada");
    ("h4rithd",
     "1f1f1e 839496 ffffff 151507 c94b16 849900 b58900 839496 d33682 b0d1ba ffffff 1f1f1e cb4b16 586e75 657b83 839496 6c71c4 93a1a1 ffffff");
    ("h4rithd.com",
     "1e1e1e fffeff ffffff 010100 cc0000 4e9b07 c4a000 3464a5 75517a 06989a d3d7cf 545752 ee282b 8be235 fde94e 729ece ac7ea8 35e2e3 d3d7cf");
    ("Hacktober",
     "141414 c9c9c9 c9c9c9 191918 b34538 587744 d08949 206ec5 864651 ac9166 f1eee7 2c2b2a b33323 42824a c75a22 5389c5 e795a5 ebc587 ffffff");
    ("Hardcore",
     "121212 a0a0a0 bbbbbb 1b1d1e f92672 a6e22e fd971f 66d9ef 9e6ffe 5e7175 ccccc6 505354 ff669d beed5f e6db74 66d9ef 9e6ffe a3babf f8f8f2");
    ("Harper",
     "010101 a8a49d a8a49d 010101 f8b63f 7fb5e1 d6da25 489e48 b296c6 f5bfd7 a8a49d 726e6a f8b63f 7fb5e1 d6da25 489e48 b296c6 f5bfd7 fefbea");
    ("HaX0R_BLUE",
     "010515 11b7ff 10b6ff 010921 10b6ff 10b6ff 10b6ff 10b6ff 10b6ff 10b6ff fafafa 080117 00b3f7 00b3f7 00b3f7 00b3f7 00b3f7 00b3f7 fefefe");
    ("HaX0R_GR33N",
     "020f01 16b10e 15d00d 001f0b 15d00d 15d00d 15d00d 15d00d 15d00d 15d00d fafafa 001510 19e20e 19e20e 19e20e 19e20e 19e20e 19e20e fefefe");
    ("HaX0R_R3D",
     "200101 b10e0e b00d0d 1f0000 b00d0d b00d0d b00d0d b00d0d b00d0d b00d0d fafafa 150000 ff1111 ff1010 ff1010 ff1010 ff1010 ff1010 fefefe");
    ("Highway",
     "222225 ededed e0d9b9 000000 d00e18 138034 ffcb3e 006bb3 6b2775 384564 ededed 5d504a f07e18 b1d130 fff120 4fc2fd de0071 5d504a ffffff");
    ("Hipster Green",
     "100b05 84c138 23ff18 000000 b6214a 00a600 bfbf00 246eb2 b200b2 00a6b2 bfbfbf 666666 e50000 86a93e e5e500 0000ff e500e5 00e5e5 e5e5e5");
    ("Hivacruz",
     "132638 ede4e4 979db4 202746 c94922 ac9739 c08b30 3d8fd1 6679cc 22a2c9 979db4 6b7394 c76b29 73ad43 5e6687 898ea4 dfe2f1 9c637a f5f7ff");
    ("Homebrew",
     "000000 00ff00 23ff18 000000 990000 00a600 999900 0000b2 b200b2 00a6b2 bfbfbf 666666 e50000 00d900 e5e500 0000ff e500e5 00e5e5 e5e5e5");
    ("Hopscotch",
     "322931 b9b5b8 b9b5b8 322931 dd464c 8fc13e fdcc59 1290bf c85e7c 149b93 b9b5b8 797379 fd8b19 433b42 5c545b 989498 d5d3d5 b33508 ffffff");
    ("Hopscotch.256",
     "322931 b9b5b8 b9b5b8 322931 dd464c 8fc13e fdcc59 1290bf c85e7c 149b93 b9b5b8 797379 dd464c 8fc13e fdcc59 1290bf c85e7c 149b93 ffffff");
    ("Horizon",
     "1c1e26 bdc0c2 ffffff 0a0a0d e95678 29d398 fab795 26bbd9 ee64ac 59e1e3 e5e5e5 848484 ec6a88 3fdaa4 fbc3a7 3fc4de f075b5 6be4e6 e5e5e5");
    ("Hurtado",
     "000000 dbdbdb bbbbbb 575757 ff1b00 a5e055 fbe74a 496487 fd5ff1 86e9fe cbcccb 262626 d51d00 a5df55 fbe84a 89beff c001c1 86eafe dbdbdb");
    ("Hybrid",
     "161719 b7bcba b7bcba 2a2e33 b84d51 b3bf5a e4b55e 6e90b0 a17eac 7fbfb4 b5b9b6 1d1f22 8d2e32 798431 e58a50 4b6b88 6e5079 4d7b74 5a626a");
    ("Hyper",
     "000000 ffffff ffffff 000000 fe0100 33ff00 feff00 0066ff cc00ff 00ffff d0d0d0 808080 fe0100 33ff00 feff00 0066ff cc00ff 00ffff ffffff");
    ("iceberg-dark",
     "161821 c6c8d1 c6c8d1 1e2132 e27878 b4be82 e2a478 84a0c6 a093c7 89b8c2 c6c8d1 6b7089 e98989 c0ca8e e9b189 91acd1 ada0d3 95c4ce d2d4de");
    ("iceberg-light",
     "e8e9ec 33374c 33374c dcdfe7 cc517a 668e3d c57339 2d539e 7759b4 3f83a6 33374c 8389a3 cc3768 598030 b6662d 22478e 6845ad 327698 262a3f");
    ("IC_Green_PPL",
     "2c2c2c e0f1dc 47fa6b 014401 ff2736 41a638 76a831 2ec3b9 50a096 3ca078 e6fef2 035c03 b4fa5c aefb86 dafa87 2efaeb 50fafa 3cfac8 e0f1dc");
    ("IC_Orange_PPL",
     "262626 ffcb83 fc531d 000000 c13900 a4a900 caaf00 bd6d00 fc5e00 f79500 ffc88a 6a4f2a ff8c68 f6ff40 ffe36e ffbe55 fc874f c69752 fafaff");
    ("idea",
     "202020 adadad bbbbbb adadad fc5256 98b61c ccb444 437ee7 9d74b0 248887 181818 ffffff fc7072 98b61c ffff0b 6c9ced fc7eff 248887 181818");
    ("idleToes",
     "323232 ffffff d6d6d6 323232 d25252 7fe173 ffc66d 4099ff f680ff bed6ff eeeeec 535353 f07070 9dff91 ffe48b 5eb7f7 ff9dff dcf4ff ffffff");
    ("IR_Black",
     "000000 f1f1f1 808080 4f4f4f fa6c60 a8ff60 fffeb7 96cafe fa73fd c6c5fe efedef 7b7b7b fcb6b0 cfffab ffffcc b5dcff fb9cfe e0e0fe ffffff");
    ("iTerm2 Dark Background",
     "000000 c7c7c7 c7c7c7 000000 c91b00 00c200 c7c400 0225c7 ca30c7 00c5c7 c7c7c7 686868 ff6e67 5ffa68 fffc67 6871ff ff77ff 60fdff ffffff");
    ("iTerm2 Default",
     "000000 ffffff e5e5e5 000000 c91b00 00c200 c7c400 2225c4 ca30c7 00c5c7 ffffff 686868 ff6e67 5ffa68 fffc67 6871ff ff77ff 60fdff ffffff");
    ("iTerm2 Light Background",
     "ffffff 000000 000000 000000 c91b00 00c200 c7c400 0225c7 ca30c7 00c5c7 c7c7c7 686868 ff6e67 5ffa68 fffc67 6871ff ff77ff 60fdff ffffff");
    ("iTerm2 Pastel Dark Background",
     "000000 c7c7c7 ffb473 626262 ff8373 b4fb73 fffdc3 a5d5fe ff90fe d1d1fe f1f1f1 8f8f8f ffc4be d6fcba fffed5 c2e3ff ffb2fe e6e6fe ffffff");
    ("iTerm2 Smoooooth",
     "15191f dcdcdc ffffff 14191e b43c2a 00c200 c7c400 2744c7 c040be 00c5c7 c7c7c7 686868 dd7975 58e790 ece100 a7abf2 e17ee1 60fdff ffffff");
    ("iTerm2 Solarized Dark",
     "002b36 839496 839496 073642 dc322f 859900 b58900 268bd2 d33682 2aa198 eee8d5 002b36 cb4b16 586e75 657b83 839496 6c71c4 93a1a1 fdf6e3");
    ("iTerm2 Solarized Light",
     "fdf6e3 657b83 657b83 073642 dc322f 859900 b58900 268bd2 d33682 2aa198 eee8d5 002b36 cb4b16 586e75 657b83 839496 6c71c4 93a1a1 fdf6e3");
    ("iTerm2 Tango Dark",
     "000000 ffffff ffffff 000000 d81e00 5ea702 cfae00 427ab3 89658e 00a7aa dbded8 686a66 f54235 99e343 fdeb61 84b0d8 bc94b7 37e6e8 f1f1f0");
    ("iTerm2 Tango Light",
     "ffffff 000000 000000 000000 d81e00 5ea702 cfae00 427ab3 89658e 00a7aa dbded8 686a66 f54235 99e343 fdeb61 84b0d8 bc94b7 37e6e8 f1f1f0");
    ("Jackie Brown",
     "2c1d16 ffcc2f 23ff18 2c1d16 ef5734 2baf2b bebf00 246eb2 d05ec1 00acee bfbfbf 666666 e50000 86a93e e5e500 0000ff e500e5 00e5e5 e5e5e5");
    ("Japanesque",
     "1e1e1e f7f6ec edcf4f 343935 cf3f61 7bb75b e9b32a 4c9ad4 a57fc4 389aad fafaf6 595b59 d18fa6 767f2c 78592f 135979 604291 76bbca b2b5ae");
    ("Jellybeans",
     "121212 dedede ffa560 929292 e27373 94b979 ffba7b 97bedc e1c0fa 00988e dedede bdbdbd ffa1a1 bddeab ffdca0 b1d8f6 fbdaff 1ab2a8 ffffff");
    ("JetBrains Darcula",
     "202020 adadad ffffff 000000 fa5355 126e00 c2c300 4581eb fa54ff 33c2c1 adadad 555555 fb7172 67ff4f ffff00 6d9df1 fb82ff 60d3d1 eeeeee");
    ("jubi",
     "262b33 c3d3de c3d3de 3b3750 cf7b98 90a94b 6ebfc0 576ea6 bc4f68 75a7d2 c3d3de a874ce de90ab bcdd61 87e9ea 8c9fcd e16c87 b7c9ef d5e5f1");
    ("Juicy Colors",
     "292a24 cccccc ffffff 292a24 ff6249 68d644 fcc223 209de5 af80ff 00d2ff cccccc 808080 ff6249 68d644 fcc223 209de5 af80ff 00d2ff f2f2f2");
    ("Kanagawa",
     "1f1f28 dcd7ba dcd7ba 1f1f28 e82424 76946a ff9e3b 658594 957fb8 9cabca dcd7ba 2a2a37 ff5d62 98bb6c e6c384 7fb4ca d27e99 a3d4d5 dcd7ba");
    ("kanagawabones",
     "1f1f28 ddd8bb e6e0c2 1f1f28 e46a78 98bc6d e5c283 7eb3c9 957fb8 7eb3c9 ddd8bb 3c3c51 ec818c 9ec967 f1c982 7bc2df a98fd2 7bc2df a8a48d");
    ("Kibble",
     "0e100a f7f7f7 9fda9c 4d4d4d c70031 29cf13 d8e30e 3449d1 8400ff 0798ab e2d1e3 5a5a5a f01578 6ce05c f3f79e 97a4f7 c495f0 68f2e0 ffffff");
    ("Kolorit",
     "1d1a1e efecec c7c7c7 1d1a1e ff5b82 47d7a1 e8e562 5db4ee da6cda 57e9eb ededed 1d1a1e ff5b82 47d7a1 e8e562 5db4ee da6cda 57e9eb ededed");
    ("Konsolas",
     "060606 c8c1c1 c8c1c1 000000 aa1717 18b218 ebae1f 2323a5 ad1edc 42b0c8 c8c1c1 7b716e ff4141 5fff5f ffff55 4b4bff ff54ff 69ffff ffffff");
    ("Lab Fox",
     "2e2e2e ffffff 7f7f7f 2e2e2e fc6d26 3eb383 fca121 db3b21 380d75 6e49cb ffffff 464646 ff6517 53eaa8 fca013 db501f 441090 7d53e7 ffffff");
    ("Laser",
     "030d18 f106e3 00ff9c 626262 ff8373 b4fb73 09b4bd fed300 ff90fe d1d1fe f1f1f1 8f8f8f ffc4be d6fcba fffed5 f92883 ffb2fe e6e7fe ffffff");
    ("Later This Evening",
     "222222 959595 424242 2b2b2b d45a60 afba67 e5d289 a0bad6 c092d6 91bfb7 3c3d3d 454747 d3232f aabb39 e5be39 6699d6 ab53d6 5fc0ae c1c2c2");
    ("Lavandula",
     "050014 736e7d 8c91fa 230046 7d1625 337e6f 7f6f49 4f4a7f 5a3f7f 58777f 736e7d 372d46 e05167 52e0c4 e0c386 8e87e0 a776e0 9ad4e0 8c91fa");
    ("LiquidCarbon",
     "303030 afc2c2 ffffff 000000 ff3030 559a70 ccac00 0099cc cc69c8 7ac4cc bccccc 000000 ff3030 559a70 ccac00 0099cc cc69c8 7ac4cc bccccc");
    ("LiquidCarbonTransparent",
     "000000 afc2c2 ffffff 000000 ff3030 559a70 ccac00 0099cc cc69c8 7ac4cc bccccc 000000 ff3030 559a70 ccac00 0099cc cc69c8 7ac4cc bccccc");
    ("LiquidCarbonTransparentInverse",
     "000000 afc2c2 ffffff bccccd ff3030 559a70 ccac00 0099cc cc69c8 7ac4cc 000000 ffffff ff3030 559a70 ccac00 0099cc cc69c8 7ac4cc 000000");
    ("lovelace",
     "1d1f28 fdfdfd c574dd 282a36 f37f97 5adecd f2a272 8897f4 c574dd 79e6f3 fdfdfd 414458 ff4971 18e3c8 ff8037 556fff b043d1 3fdcee bebec1");
    ("Man Page",
     "fef49c 000000 7f7f7f 000000 cc0000 00a600 999900 0000b2 b200b2 00a6b2 cccccc 666666 e50000 00d900 e5e500 0000ff e500e5 00e5e5 e5e5e5");
    ("Mariana",
     "343d46 d8dee9 fcbb6a 000000 ec5f66 99c794 f9ae58 6699cc c695c6 5fb4b4 f7f7f7 333333 f97b58 acd1a8 fac761 85add6 d8b6d8 82c4c4 ffffff");
    ("Material",
     "eaeaea 232322 16afca 212121 b7141f 457b24 f6981e 134eb2 560088 0e717c efefef 424242 e83b3f 7aba3a ffea2e 54a4f3 aa4dbc 26bbd1 d9d9d9");
    ("MaterialDark",
     "232322 e5e5e5 16afca 212121 b7141f 457b24 f6981e 134eb2 560088 0e717c efefef 424242 e83b3f 7aba3a ffea2e 54a4f3 aa4dbc 26bbd1 d9d9d9");
    ("MaterialDarker",
     "212121 eeffff ffffff 000000 ff5370 c3e88d ffcb6b 82aaff c792ea 89ddff ffffff 545454 ff5370 c3e88d ffcb6b 82aaff c792ea 89ddff ffffff");
    ("MaterialDesignColors",
     "1d262a e7ebed eaeaea 435b67 fc3841 5cf19e fed032 37b6ff fc226e 59ffd1 ffffff a1b0b8 fc746d adf7be fee16c 70cfff fc669b 9affe6 ffffff");
    ("MaterialOcean",
     "0f111a 8f93a2 ffcc00 546e7a ff5370 c3e88d ffcb6b 82aaff c792ea 89ddff ffffff 546e7a ff5370 c3e88d ffcb6b 82aaff c792ea 89ddff ffffff");
    ("Mathias",
     "000000 bbbbbb bbbbbb 000000 e52222 a6e32d fc951e c48dff fa2573 67d9f0 f2f2f2 555555 ff5555 55ff55 ffff55 5555ff ff55ff 55ffff ffffff");
    ("matrix",
     "0f191c 426644 384545 0f191c 23755a 82d967 ffd700 3f5242 409931 50b45a 507350 688060 2fc079 90d762 faff00 4f7e7e 11ff25 c1ff8a 678c61");
    ("Medallion",
     "1d1908 cac296 d3ba30 000000 b64c00 7c8b16 d3bd26 616bb0 8c5a90 916c25 cac29a 5e5219 ff9149 b2ca3b ffe54a acb8ff ffa0ff ffbc51 fed698");
    ("midnight-in-mojave",
     "1e1e1e ffffff 32d74b 1e1e1e ff453a 32d74b ffd60a 0a84ff bf5af2 5ac8fa ffffff 1e1e1e ff453a 32d74b ffd60a 0a84ff bf5af2 5ac8fa ffffff");
    ("Mirage",
     "1b2738 a6b2c0 ddb3ff 011627 ff9999 85cc95 ffd700 7fb5ff ddb3ff 21c7a8 ffffff 575656 ff9999 85cc95 ffd700 7fb5ff ddb3ff 85cc95 ffffff");
    ("Misterioso",
     "2d3743 e1e1e0 000000 000000 ff4242 74af68 ffad29 338f86 9414e6 23d7d7 e1e1e0 555555 ff3242 74cd68 ffb929 23d7d7 ff37ff 00ede1 ffffff");
    ("Molokai",
     "121212 bbbbbb bbbbbb 121212 fa2573 98e123 dfd460 1080d0 8700ff 43a8d0 bbbbbb 555555 f6669d b1e05f fff26d 00afff af87ff 51ceff ffffff");
    ("MonaLisa",
     "120b0d f7d66a c46c32 351b0e 9b291c 636232 c36e28 515c5d 9b1d29 588056 f7d75c 874228 ff4331 b4b264 ff9566 9eb2b4 ff5b6a 8acd8f ffe598");
    ("Monokai Cmder",
     "272822 cacaca ffffff 272822 a70334 74aa04 b6b649 01549e 89569c 1a83a6 cacaca 7c7c7c f3044b 8dd006 cccc81 0383f5 a87db8 58c2e5 ffffff");
    ("Monokai Pro",
     "403e41 fcfcfa fcfcfa 403e41 ff6188 a9dc76 ffd866 fc9867 ab9df2 78dce8 fcfcfa 727072 ff6188 a9dc76 ffd866 fc9867 ab9df2 78dce8 fcfcfa");
    ("Monokai Pro (Filter Octagon)",
     "282a3a eaf2f1 ffffff 000000 d81e00 5ea702 cfae00 427ab3 89658e 00a7aa dbded8 686a66 f54235 99e343 fdeb61 84b0d8 bc94b7 37e6e8 f1f1f0");
    ("Monokai Pro (Filter Ristretto)",
     "2c2525 fff1f3 fff1f3 403838 fd6883 adda78 f9cc6c f38d70 a8a9eb 85dacc fff1f3 72696a fd6883 adda78 f9cc6c f38d70 a8a9eb 85dacc fff1f3");
    ("Monokai Remastered",
     "0c0c0c d9d9d9 fc971f 1a1a1a f4005f 98e024 fd971f 9d65ff f4005f 58d1eb c4c5b5 625e4c f4005f 98e024 e0d561 9d65ff f4005f 58d1eb f6f6ef");
    ("Monokai Soda",
     "1a1a1a c4c5b5 f6f7ec 1a1a1a f4005f 98e024 fa8419 9d65ff f4005f 58d1eb c4c5b5 625e4c f4005f 98e024 e0d561 9d65ff f4005f 58d1eb f6f6ef");
    ("Monokai Vivid",
     "121212 f9f9f9 fb0007 121212 fa2934 98e123 fff30a 0443ff f800f8 01b6ed ffffff 838383 f6669d b1e05f fff26d 0443ff f200f6 51ceff ffffff");
    ("Moonlight II",
     "222436 c8d3f5 ffffff 191a2a ff757f c3e88d ffc777 82aaff c099ff 86e1fc c8d3f5 828bb8 ff757f c3e88d ffc777 82aaff c099ff 86e1fc c8d3f5");
    ("N0tch2k",
     "222222 a0a0a0 aa9175 383838 a95551 666666 a98051 657d3e 767676 c9c9c9 d0b8a3 474747 a97775 8c8c8c a99175 98bd5e a3a3a3 dcdcdc d8c8bb");
    ("neobones_dark",
     "0f191f c6d5cf ceddd7 0f191f de6e7c 90ff6b b77e64 8190d4 b279a7 66a5ad c6d5cf 263945 e8838f a0ff85 d68c67 92a0e2 cf86c1 65b8c1 98a39e");
    ("neobones_light",
     "e5ede6 202e18 202e18 e5ede6 a8334c 567a30 944927 286486 88507d 3b8992 202e18 b3c6b6 94253e 3f5a22 803d1c 1d5573 7b3b70 2b747c 415934");
    ("Neon",
     "14161a 00fffc c7c7c7 000000 ff3045 5ffa74 fffc7e 0208cb f924e7 00fffc c7c7c7 686868 ff5a5a 75ff88 fffd96 3c40cb f15be5 88fffe ffffff");
    ("Neopolitan",
     "271f19 ffffff ffffff 000000 800000 61ce3c fbde2d 253b76 ff0080 8da6ce f8f8f8 000000 800000 61ce3c fbde2d 253b76 ff0080 8da6ce f8f8f8");
    ("Neutron",
     "1c1e22 e6e8ef f6f7ec 23252b b54036 5ab977 deb566 6a7c93 a4799d 3f94a8 e6e8ef 23252b b54036 5ab977 deb566 6a7c93 a4799d 3f94a8 ebedf2");
    ("Night Owlish Light",
     "ffffff 403f53 403f53 011627 d3423e 2aa298 daaa01 4876d6 403f53 08916a 7a8181 7a8181 f76e6e 49d0c5 dac26b 5ca7e4 697098 00c990 989fb1");
    ("NightLion v1",
     "000000 bbbbbb bbbbbb 4c4c4c bb0000 5fde8f f3f167 276bd8 bb00bb 00dadf bbbbbb 555555 ff5555 55ff55 ffff55 5555ff ff55ff 55ffff ffffff");
    ("NightLion v2",
     "171717 bbbbbb bbbbbb 4c4c4c bb0000 04f623 f3f167 64d0f0 ce6fdb 00dadf bbbbbb 555555 ff5555 7df71d ffff55 62cbe8 ff9bf5 00ccd8 ffffff");
    ("niji",
     "141515 ffffff ffc663 333333 d23e08 54ca74 fff700 2ab9ff ff50da 1ef9f5 ddd0c4 515151 ffb7b7 c1ffae fcffb8 8efff3 ffa2ed bcffc7 ffffff");
    ("Nocturnal Winter",
     "0d0d17 e6e5e5 e6e5e5 4d4d4d f12d52 09cd7e f5f17a 3182e0 ff2b6d 09c87a fcfcfc 808080 f16d86 0ae78d fffc67 6096ff ff78a2 0ae78d ffffff");
    ("nord",
     "2e3440 d8dee9 eceff4 3b4252 bf616a a3be8c ebcb8b 81a1c1 b48ead 88c0d0 e5e9f0 4c566a bf616a a3be8c ebcb8b 81a1c1 b48ead 8fbcbb eceff4");
    ("nord-light",
     "e5e9f0 414858 88c0d0 3b4252 bf616a a3be8c ebcb8b 81a1c1 b48ead 88c0d0 d8dee9 4c566a bf616a a3be8c ebcb8b 81a1c1 b48ead 8fbcbb eceff4");
    ("Novel",
     "dfdbc3 3b2322 73635a 000000 cc0000 009600 d06b00 0000cc cc00cc 0087cc cccccc 808080 cc0000 009600 d06b00 0000cc cc00cc 0087cc ffffff");
    ("Obsidian",
     "283033 cdcdcd c0cad0 000000 a60001 00bb00 fecd22 3a9bdb bb00bb 00bbbb bbbbbb 555555 ff0003 93c863 fef874 a1d7ff ff55ff 55ffff ffffff");
    ("Ocean",
     "224fbc ffffff 7f7f7f 000000 990000 00a600 999900 0000b2 b200b2 00a6b2 bfbfbf 666666 e50000 00d900 e5e500 0000ff e500e5 00e5e5 e5e5e5");
    ("Oceanic-Next",
     "121b21 b3b8c3 b3b8c3 121c21 e44754 89bd82 f7bd51 5486c0 b77eb8 50a5a4 ffffff 52606b e44754 89bd82 f7bd51 5486c0 b77eb8 50a5a4 ffffff");
    ("OceanicMaterial",
     "1c262b c2c8d7 b3b8c3 000000 ee2b2a 40a33f ffea2e 1e80f0 8800a0 16afca a4a4a4 777777 dc5c60 70be71 fff163 54a4f3 aa4dbc 42c7da ffffff");
    ("Ollie",
     "222125 8a8dae 5b6ea7 000000 ac2e31 31ac61 ac4300 2d57ac b08528 1fa6ac 8a8eac 5b3725 ff3d48 3bff99 ff5e1e 4488ff ffc21d 1ffaff 5b6ea7");
    ("OneDark",
     "1e2127 abb2bf ffffff 1e2127 e06c75 98c379 d19a66 61afef c678dd 56b6c2 abb2bf 5c6370 e06c75 98c379 d19a66 61afef c678dd 56b6c2 ffffff");
    ("OneHalfDark",
     "282c34 dcdfe4 a3b3cc 282c34 e06c75 98c379 e5c07b 61afef c678dd 56b6c2 dcdfe4 282c34 e06c75 98c379 e5c07b 61afef c678dd 56b6c2 dcdfe4");
    ("OneHalfLight",
     "fafafa 383a42 bfceff 383a42 e45649 50a14f c18401 0184bc a626a4 0997b3 fafafa 4f525e e06c75 98c379 e5c07b 61afef c678dd 56b6c2 ffffff");
    ("OneStar",
     "0e0e0e e4e4e4 ffffff 000000 d13b3b 0dbc79 dfdf44 2472c8 c42cc4 33a0bb f1f1f1 666666 fa4b4b 23d18b fcfc5c 3b8eea d861d8 29b8db fafafa");
    ("Operator Mono Dark",
     "191919 c3cac2 fcdc08 5a5a5a ca372d 4d7b3a d4d697 4387cf b86cb4 72d5c6 ced4cd 9a9b99 c37d62 83d0a2 fdfdc5 89d3f6 ff2c7a 82eada fdfdf6");
    ("Overnight Slumber",
     "0e1729 ced2d6 ffa7c4 0a1222 ffa7c4 85cc95 ffcb8b 8dabe1 c792eb 78ccf0 ffffff 575656 ffa7c4 85cc95 ffcb8b 8dabe1 c792eb ffa7c4 ffffff");
    ("PaleNightHC",
     "3e4251 cccccc ffcb6b 000000 f07178 c3e88d ffcb6b 82aaff c792ea 89ddff ffffff 666666 f6a9ae dbf1ba ffdfa6 b4ccff ddbdf2 b8eaff 999999");
    ("Pandora",
     "141e43 e1e1e1 43d58e 000000 ff4242 74af68 ffad29 338f86 9414e6 23d7d7 e2e2e2 3f5648 ff3242 74cd68 ffb929 23d7d7 ff37ff 00ede1 ffffff");
    ("Paraiso Dark",
     "2f1e2e a39e9b a39e9b 2f1e2e ef6155 48b685 fec418 06b6ef 815ba4 5bc4bf a39e9b 776e71 ef6155 48b685 fec418 06b6ef 815ba4 5bc4bf e7e9db");
    ("PaulMillr",
     "000000 f2f2f2 4d4d4d 2a2a2a ff0000 79ff0f e7bf00 396bd7 b449be 66ccff bbbbbb 666666 ff0080 66ff66 f3d64e 709aed db67e6 7adff2 ffffff");
    ("PencilDark",
     "212121 f1f1f1 20bbfc 212121 c30771 10a778 a89c14 008ec4 523c79 20a5ba d9d9d9 424242 fb007a 5fd7af f3e430 20bbfc 6855de 4fb8cc f1f1f1");
    ("PencilLight",
     "f1f1f1 424242 20bbfc 212121 c30771 10a778 a89c14 008ec4 523c79 20a5ba d9d9d9 424242 fb007a 5fd7af f3e430 20bbfc 6855de 4fb8cc f1f1f1");
    ("Peppermint",
     "000000 c8c8c8 bbbbbb 353535 e74669 89d287 dab853 449fd0 da62dc 65aaaf b4b4b4 535353 e4859b a3cca2 e1e487 6fbce2 e586e7 96dcdb dfdfdf");
    ("Piatto Light",
     "ffffff 414141 5e77c8 414141 b23771 66781e cd6f34 3c5ea8 a454b2 66781e ffffff 3f3f3f db3365 829429 cd6f34 3c5ea8 a454b2 829429 f2f2f2");
    ("Pnevma",
     "1c1c1c d0d0d0 e4c9af 2f2e2d a36666 90a57d d7af87 7fa5bd c79ec4 8adbb4 d0d0d0 4a4845 d78787 afbea2 e4c9af a1bdce d7beda b1e7dd efefef");
    ("Popping and Locking",
     "181921 ebdbb2 c7c7c7 1d2021 cc241d 98971a d79921 458588 b16286 689d6a a89984 928374 f42c3e b8bb26 fabd2f 99c6ca d3869b 7ec16e ebdbb2");
    ("primary",
     "ffffff 000000 000000 000000 db4437 0f9d58 f4b400 4285f4 db4437 4285f4 ffffff 000000 db4437 0f9d58 f4b400 4285f4 4285f4 0f9d58 ffffff");
    ("Primer",
     "1a2b3c ffffff ffffff 000000 ea4a5a 34d058 ffdf5d 2188ff 8a63d2 15e2e2 ecf0f1 4f5861 fdaeb7 bef5cb fff5b1 c8e1ff d1bcf9 a2ecec ffffff");
    ("Pro",
     "000000 f2f2f2 4d4d4d 000000 990000 00a600 999900 2009db b200b2 00a6b2 bfbfbf 666666 e50000 00d900 e5e500 0000ff e500e5 00e5e5 e5e5e5");
    ("Pro Light",
     "ffffff 191919 4d4d4d 000000 e5492b 50d148 c6c440 3b75ff ed66e8 4ed2de dcdcdc 9f9f9f ff6640 61ef57 f2f156 0082ff ff7eff 61f7f8 f2f2f2");
    ("Purple Rain",
     "21084a fffbf6 ff271d 000000 ff260e 9be205 ffc400 00a2fa 815bb5 00deef ffffff 565656 ff4250 b8e36e ffd852 00a6ff ac7bf0 74fdf3 ffffff");
    ("purplepeter",
     "2a1a4a ece7fa c7c7c7 0a0520 ff796d 99b481 efdfac 66d9ef e78fcd ba8cff ffba81 100b23 f99f92 b4be8f f2e9bf 79daed ba91d4 a0a0d6 b9aed3");
    ("QB64 Super Dark Blue",
     "000027 d8d8d8 d8d8d8 000000 98220e 157e15 808000 054663 631563 00485c 989898 626262 d8624e 55ce55 ffa700 457693 934593 00586c d8d8d8");
    ("Rapture",
     "111e2a c0c9e5 ffffff 000000 fc644d 7afde1 fff09b 6c9bf5 ff4fa1 64e0ff c0c9e5 304b66 fc644d 7afde1 fff09b 6c9bf5 ff4fa1 64e0ff ffffff");
    ("Raycast_Dark",
     "1a1a1a ffffff cccccc 000000 ff5360 59d499 ffc531 56c2ff cf2f98 52eee5 ffffff 000000 ff6363 59d499 ffc531 56c2ff cf2f98 52eee5 ffffff");
    ("Raycast_Light",
     "ffffff 000000 000000 000000 b12424 006b4f f8a300 138af2 9a1b6e 3eb8bf ffffff 000000 b12424 006b4f f8a300 138af2 9a1b6e 3eb8bf ffffff");
    ("rebecca",
     "292a44 e8e6ed b89bf9 12131e dd7755 04dbb5 f2e7b7 7aa5ff bf9cf9 56d3c2 e4e3e9 666699 ff92cd 01eac0 fffca8 69c0fa c17ff8 8bfde1 f4f2f9");
    ("Red Alert",
     "762423 ffffff ffffff 000000 d62e4e 71be6b beb86b 489bee e979d7 6bbeb8 d6d6d6 262626 e02553 aff08c dfddb7 65aaf1 ddb7df b7dfdd ffffff");
    ("Red Planet",
     "222222 c2b790 c2b790 202020 8c3432 728271 e8bf6a 69819e 896492 5b8390 b9aa99 676767 b55242 869985 ebeb91 60827e de4974 38add8 d6bfb8");
    ("Red Sands",
     "7a251e d7c9a7 ffffff 000000 ff3f00 00bb00 e7b000 0072ff bb00bb 00bbbb bbbbbb 555555 bb0000 00bb00 e7b000 0072ae ff55ff 55ffff ffffff");
    ("Relaxed",
     "353a44 d9d9d9 d9d9d9 151515 bc5653 909d63 ebc17a 6a8799 b06698 c9dfff d9d9d9 636363 bc5653 a0ac77 ebc17a 7eaac7 b06698 acbbd0 f7f7f7");
    ("Retro",
     "000000 13a10e 13a10e 13a10e 13a10e 13a10e 13a10e 13a10e 13a10e 13a10e 13a10e 16ba10 16ba10 16ba10 16ba10 16ba10 16ba10 16ba10 16ba10");
    ("Retrowave",
     "070825 46bdff ffffff 181a1f ff16b0 929292 fcee54 46bdff ff92df df81fc ffffff ff16b0 f85353 fcee54 ffffff 46bdff ff92df ff901f ffffff");
    ("Rippedcasts",
     "2b2b2b ffffff 7f7f7f 000000 cdaf95 a8ff60 bfbb1f 75a5b0 ff73fd 5a647e bfbfbf 666666 eecbad bcee68 e5e500 86bdc9 e500e5 8c9bc4 e5e5e5");
    ("Rose Pine",
     "191724 e0def4 ffffff 706e86 eb6f92 9ccfd8 f6c177 31748f c4a7e7 ebbcba e0def4 706e86 eb6f92 9ccfd8 f6c177 31748f c4a7e7 ebbcba e0def4");
    ("rose-pine",
     "191724 e0def4 e0def4 26233a eb6f92 9ccfd8 f6c177 31748f c4a7e7 ebbcba e0def4 6e6a86 eb6f92 9ccfd8 f6c177 31748f c4a7e7 ebbcba e0def4");
    ("rose-pine-dawn",
     "faf4ed 575279 575279 f2e9e1 b4637a 56949f ea9d34 286983 907aa9 d7827e 575279 9893a5 b4637a 56949f ea9d34 286983 907aa9 d7827e 575279");
    ("rose-pine-moon",
     "232136 e0def4 e0def4 393552 eb6f92 9ccfd8 f6c177 3e8fb0 c4a7e7 ea9a97 e0def4 6e6a86 eb6f92 9ccfd8 f6c177 3e8fb0 c4a7e7 ea9a97 e0def4");
    ("Rouge 2",
     "17182b a2a3aa 969e92 5d5d6b c6797e 969e92 dbcdab 6e94b9 4c4e78 8ab6c1 e8e8ea 616274 c6797e e6dcc4 e6dcc4 98b3cd 8283a1 abcbd3 e8e8ea");
    ("Royal",
     "100815 514968 524966 241f2b 91284c 23801c b49d27 6580b0 674d96 8aaabe 524966 312d3d d5356c 2cd946 fde83b 90baf9 a479e3 acd4eb 9e8cbd");
    ("Ryuuko",
     "2c3941 ececec ececec 2c3941 865f5b 66907d b1a990 6a8e95 b18a73 88b2ac ececec 5d7079 865f5b 66907d b1a990 6a8e95 b18a73 88b2ac ececec");
    ("Sakura",
     "18131e dd7bdc ff65fd 000000 d52370 41af1a bc7053 6964ab c71fbf 939393 998eac 786d69 f41d99 22e529 f59574 9892f1 e90cdd eeeeee cbb6ff");
    ("Scarlet Protocol",
     "1c153d e41951 76ff9f 101116 ff0051 00dc84 faf945 0271b6 ca30c7 00c5c7 c7c7c7 686868 ff6e67 5ffa68 fffc67 6871ff bd35ec 60fdff ffffff");
    ("Seafoam Pastel",
     "243435 d4e7d4 57647a 757575 825d4d 728c62 ada16d 4d7b82 8a7267 729494 e0e0e0 8a8a8a cf937a 98d9aa fae79d 7ac3cf d6b2a1 ade0e0 e0e0e0");
    ("SeaShells",
     "09141b deb88d fca02f 17384c d15123 027c9b fca02f 1e4950 68d4f1 50a3b5 deb88d 434b53 d48678 628d98 fdd39f 1bbcdd bbe3ee 87acb4 fee4ce");
    ("seoulbones_dark",
     "4b4b4b dddddd e2e2e2 4b4b4b e388a3 98bd99 ffdf9b 97bdde a5a6c5 6fbdbe dddddd 6c6465 eb99b1 8fcd92 ffe5b3 a2c8e9 b2b3da 6bcacb a8a8a8");
    ("seoulbones_light",
     "e2e2e2 555555 555555 e2e2e2 dc5284 628562 c48562 0084a3 896788 008586 555555 bfbabb be3c6d 487249 a76b48 006f89 7f4c7e 006f70 777777");
    ("Serendipity Midnight",
     "1c1e2d dee0ef 6b6d7c 232534 ee8679 9ccfd8 f8d2c9 5ba2d0 a78bfa 94b8ff dee0ef 8d8f9e ee8679 9ccfd8 f8d2c9 5ba2d0 a78bfa 94b8ff dee0ef");
    ("Serendipity Morning",
     "fffaf3 575279 9893a5 f2e9de d26a5d 77aab3 c8a299 3788be 886cdb 7397de 575279 6e6a86 d26a5d 77aab3 c8a299 3788be 886cdb 7397de 575279");
    ("Serendipity Sunset",
     "272938 dee0ef 8d8f9e 363847 d1918f aac9d4 edd5d6 709bbd a392dc a0b6e8 dee0ef 6b6d7c d1918f aac9d4 edd5d6 709bbd a392dc a0b6e8 dee0ef");
    ("Seti",
     "111213 cacecd e3bf21 323232 c22832 8ec43d e0c64f 43a5d5 8b57b5 8ec43d eeeeee 323232 c22832 8ec43d e0c64f 43a5d5 8b57b5 8ec43d ffffff");
    ("shades-of-purple",
     "1e1d40 ffffff fad000 000000 d90429 3ad900 ffe700 6943ff ff2c70 00c5c7 c7c7c7 686868 f92a1c 43d426 f1d000 6871ff ff77ff 79e8fb ffffff");
    ("Shaman",
     "001015 405555 4afcd6 012026 b2302d 00a941 5e8baa 449a86 00599d 5d7e19 405555 384451 ff4242 2aea5e 8ed4fd 61d5ba 1298ff 98d028 58fbd6");
    ("Slate",
     "222222 35b1d2 87d3c4 222222 e2a8bf 81d778 c4c9c0 264b49 a481d3 15ab9c 02c5e0 ffffff ffcdd9 beffa8 d0ccca 7ab0d2 c5a7d9 8cdfe0 e0e0e0");
    ("SleepyHollow",
     "121214 af9a91 af9a91 572100 ba3934 91773f b55600 5f63b4 a17c7b 8faea9 af9a91 4e4b61 d9443f d6b04e f66813 8086ef e2c2bb a4dce7 d2c7a9");
    ("Smyck",
     "1b1b1b f7f7f7 bbbbbb 000000 b84131 7da900 c4a500 62a3c4 ba8acc 207383 a1a1a1 7a7a7a d6837c c4f137 fee14d 8dcff0 f79aff 6ad9cf f7f7f7");
    ("Snazzy",
     "1e1f29 ebece6 e4e4e4 000000 fc4346 50fb7c f0fb8c 49baff fc4cb4 8be9fe ededec 555555 fc4346 50fb7c f0fb8c 49baff fc4cb4 8be9fe ededec");
    ("SoftServer",
     "242626 99a3a2 d2e0de 000000 a2686a 9aa56a a3906a 6b8fa3 6a71a3 6ba58f 99a3a2 666c6c dd5c60 bfdf55 deb360 62b1df 606edf 64e39c d2e0de");
    ("Solarized Darcula",
     "3d3f41 d2d8d9 708284 25292a f24840 629655 b68800 2075c7 797fd4 15968d d2d8d9 25292a f24840 629655 b68800 2075c7 797fd4 15968d d2d8d9");
    ("Solarized Dark - Patched",
     "001e27 708284 708284 002831 d11c24 738a05 a57706 2176c7 c61c6f 259286 eae3cb 475b62 bd3613 475b62 536870 708284 5956ba 819090 fcf4dc");
    ("Solarized Dark Higher Contrast",
     "001e27 9cc2c3 f34b00 002831 d11c24 6cbe6c a57706 2176c7 c61c6f 259286 eae3cb 006488 f5163b 51ef84 b27e28 178ec8 e24d8e 00b39e fcf4dc");
    ("Sonoran Gothic",
     "241f17 eaf4de eaf4de 867f6e eb6f6f 669c50 f2d696 227b4d a5abda e18f62 eaf4de 867f6e eb6f6f 669c50 f2d696 227b4d a5abda e18f62 eaf4de");
    ("Sonoran Sunrise",
     "f9fae3 61543e 61543e f9fae3 eb6f6f 669c50 f2c55c 227b4d 7189d9 e07941 665e4b f9fae3 eb6f6f 669c50 f2c55c 227b4d 7189d9 e07941 665e4b");
    ("Spacedust",
     "0a1e24 ecf0c1 708284 6e5346 e35b00 5cab96 e3cd7b 0f548b e35b00 06afc7 f0f1ce 684c31 ff8a3a aecab8 ffc878 67a0ce ff8a3a 83a7b4 fefff1");
    ("SpaceGray",
     "20242d b3b8c3 b3b8c3 000000 b04b57 87b379 e5c179 7d8fa4 a47996 85a7a5 b3b8c3 000000 b04b57 87b379 e5c179 7d8fa4 a47996 85a7a5 ffffff");
    ("SpaceGray Eighties",
     "222222 bdbaae bbbbbb 15171c ec5f67 81a764 fec254 5486c0 bf83c1 57c2c1 efece7 555555 ff6973 93d493 ffd256 4d84d1 ff55ff 83e9e4 ffffff");
    ("SpaceGray Eighties Dull",
     "222222 c9c6bc bbbbbb 15171c b24a56 92b477 c6735a 7c8fa5 a5789e 80cdcb b3b8c3 555555 ec5f67 89e986 fec254 5486c0 bf83c1 58c2c1 ffffff");
    ("Spiderman",
     "1b1d1e e3e3e3 2c3fff 1b1d1e e60813 e22928 e24756 2c3fff 2435db 3256ff fffef6 505354 ff0325 ff3338 fe3a35 1d50ff 747cff 6184ff fffff9");
    ("Spring",
     "ffffff 4d4d4c 4d4d4c 000000 ff4d83 1f8c3b 1fc95b 1dd3ee 8959a8 3e999f ffffff 000000 ff0021 1fc231 d5b807 15a9fd 8959a8 3e999f ffffff");
    ("Square",
     "1a1a1a acacab fcfbcc 050505 e9897c b6377d ecebbe a9cdeb 75507b c9caec f2f2f2 141414 f99286 c3f786 fcfbcc b6defb ad7fa8 d7d9fc e2e2e2");
    ("Sublette",
     "202535 ccced0 ccced0 253045 ee5577 55ee77 ffdd88 5588ff ff77cc 44eeee f5f5da 405570 ee6655 99ee77 ffff77 77bbff aa88ff 55ffbb ffffee");
    ("Subliminal",
     "282c35 d4d4d4 c7c7c7 7f7f7f e15a60 a9cfa4 ffe2a9 6699cc f1a5ab 5fb3b3 d4d4d4 7f7f7f e15a60 a9cfa4 ffe2a9 6699cc f1a5ab 5fb3b3 d4d4d4");
    ("Sundried",
     "1a1818 c9c9c9 ffffff 302b2a a7463d 587744 9d602a 485b98 864651 9c814f c9c9c9 4d4e48 aa000c 128c21 fc6a21 7999f7 fd8aa1 fad484 ffffff");
    ("Symfonic",
     "000000 ffffff dc322f 000000 dc322f 56db3a ff8400 0084d4 b729d9 ccccff ffffff 1b1d21 dc322f 56db3a ff8400 0084d4 b729d9 ccccff ffffff");
    ("synthwave",
     "000000 dad9c7 19cde6 000000 f6188f 1ebb2b fdf834 2186ec f85a21 12c3e2 ffffff 000000 f841a0 25c141 fdf454 2f9ded f97137 19cde6 ffffff");
    ("synthwave-everything",
     "2a2139 f0eff1 72f1b8 fefefe f97e72 72f1b8 fede5d 6d77b3 c792ea f772e0 fefefe fefefe f88414 72f1b8 fff951 36f9f6 e1acff f92aad fefefe");
    ("SynthwaveAlpha",
     "241b30 f2f2e3 f2f2e3 241b30 e60a70 00986c adad3e 6e29ad b300ad 00b0b1 b9b1bc 7f7094 e60a70 0ae4a4 f9f972 aa54f9 ff00f6 00fbfd f2f2e3");
    ("Tango Adapted",
     "ffffff 000000 000000 000000 ff0000 59d600 f0cb00 00a2ff c17ecc 00d0d6 e6ebe1 8f928b ff0013 93ff00 fff121 88c9ff e9a7e1 00feff f6f6f4");
    ("Tango Half Adapted",
     "ffffff 000000 000000 000000 ff0000 4cc300 e2c000 008ef6 a96cb3 00bdc3 e0e5db 797d76 ff0013 8af600 ffec00 76bfff d898d1 00f6fa f4f4f2");
    ("Teerb",
     "262626 d0d0d0 e4c9af 1c1c1c d68686 aed686 d7af87 86aed6 d6aed6 8adbb4 d0d0d0 1c1c1c d68686 aed686 e4c9af 86aed6 d6aed6 b1e7dd efefef");
    ("Terminal Basic",
     "ffffff 000000 7f7f7f 000000 990000 00a600 999900 0000b2 b200b2 00a6b2 bfbfbf 666666 e50000 00d900 e5e500 0000ff e500e5 00e5e5 e5e5e5");
    ("Thayer Bright",
     "1b1d1e f8f8f8 fc971f 1b1d1e f92672 4df840 f4fd22 2757d6 8c54fe 38c8b5 ccccc6 505354 ff5995 b6e354 feed6c 3f78ff 9e6ffe 23cfd5 f8f8f2");
    ("The Hulk",
     "1b1d1e b5b5b5 16b61b 1b1d1e 269d1b 13ce30 63e457 2525f5 641f74 378ca9 d9d8d1 505354 8dff2a 48ff77 3afe16 506b95 72589d 4085a6 e5e6e1");
    ("Tinacious Design (Dark)",
     "1d1d26 cbcbf0 cbcbf0 1d1d26 ff3399 00d364 ffcc66 00cbff cc66ff 00ceca cbcbf0 636667 ff2f92 00d364 ffd479 00cbff d783ff 00d5d4 d5d6f3");
    ("Tinacious Design (Light)",
     "f8f8ff 1d1d26 cbcbf0 1d1d26 ff3399 00d364 ffcc66 00cbff cc66ff 00ceca cbcbf0 636667 ff2f92 00d364 ffd479 00cbff d783ff 00d5d4 d5d6f3");
    ("TokyoNight",
     "16161e 787c99 ffffff 363b54 f7768e 41a6b5 e0af68 7aa2f7 bb9af7 7dcfff 787c99 363b54 f7768e 41a6b5 e0af68 7aa2f7 bb9af7 7dcfff acb0d0");
    ("tokyonight",
     "1a1b26 c0caf5 c0caf5 15161e f7768e 9ece6a e0af68 7aa2f7 bb9af7 7dcfff a9b1d6 414868 f7768e 9ece6a e0af68 7aa2f7 bb9af7 7dcfff c0caf5");
    ("tokyonight-day",
     "e1e2e7 3760bf 3760bf e9e9ed f52a65 587539 8c6c3e 2e7de9 9854f1 007197 6172b0 a1a6c5 f52a65 587539 8c6c3e 2e7de9 9854f1 007197 3760bf");
    ("tokyonight-storm",
     "24283b c0caf5 c0caf5 1d202f f7768e 9ece6a e0af68 7aa2f7 bb9af7 7dcfff a9b1d6 414868 f7768e 9ece6a e0af68 7aa2f7 bb9af7 7dcfff c0caf5");
    ("TokyoNightLight",
     "cbccd1 4c505e ffffff 0f0f14 8c4351 33635c 8f5e15 34548a 5a4a78 0f4b6e 828594 0f0f14 8c4351 33635c 8f5e15 34548a 5a4a78 0f4b6e 828594");
    ("TokyoNightStorm",
     "1f2335 7982a9 ffffff 3b4261 f7768e 73daca e0af68 7aa2f7 bb9af7 7dcfff 7982a9 3b4261 f7768e 73daca e0af68 7aa2f7 bb9af7 7dcfff a9b1d6");
    ("Tomorrow",
     "ffffff 4d4d4c 4d4d4c e0e0e0 c82829 718c00 eab700 4271ae 8959a8 3e999f 282a2e d6d6d6 c82829 718c00 eab700 4271ae 8959a8 3e999f 1d1f21");
    ("Tomorrow Night",
     "1d1f21 c5c8c6 c5c8c6 282a2e cc6666 b5bd68 f0c674 81a2be b294bb 8abeb7 e0e0e0 373b41 cc6666 b5bd68 f0c674 81a2be b294bb 8abeb7 ffffff");
    ("Tomorrow Night Blue",
     "002451 ffffff ffffff 000000 ff9da4 d1f1a9 ffeead bbdaff ebbbff 99ffff ffffff 000000 ff9da4 d1f1a9 ffeead bbdaff ebbbff 99ffff ffffff");
    ("Tomorrow Night Bright",
     "000000 eaeaea eaeaea 000000 d54e53 b9ca4a e7c547 7aa6da c397d8 70c0b1 ffffff 000000 d54e53 b9ca4a e7c547 7aa6da c397d8 70c0b1 ffffff");
    ("Tomorrow Night Burns",
     "151515 a1b0b8 ff443e 252525 832e31 a63c40 d3494e fc595f df9395 ba8586 f5f5f5 5d6f71 832e31 a63c40 d2494e fc595f df9395 ba8586 f5f5f5");
    ("Tomorrow Night Eighties",
     "2d2d2d cccccc cccccc 393939 f2777a 99cc99 ffcc66 6699cc cc99cc 66cccc e0e0e0 515151 f2777a 99cc99 ffcc66 6699cc cc99cc 66cccc ffffff");
    ("ToyChest",
     "24364b 31d07b d5d5d5 2c3f58 be2d26 1a9172 db8e27 325d96 8a5edc 35a08f 23d183 336889 dd5944 31d07b e7d84b 34a6da ae6bdc 42c3ae d5d5d5");
    ("Treehouse",
     "191919 786b53 fac814 321300 b2270e 44a900 aa820c 58859a 97363d b25a1e 786b53 433626 ed5d20 55f238 f2b732 85cfed e14c5a f07d14 ffc800");
    ("Twilight",
     "141414 ffffd4 ffffff 141414 c06d44 afb97a c2a86c 44474a b4be7c 778385 ffffd4 262626 de7c4c ccd88c e2c47e 5a5e62 d0dc8e 8a989b ffffd4");
    ("Ubuntu",
     "300a24 eeeeec bbbbbb 2e3436 cc0000 4e9a06 c4a000 3465a4 75507b 06989a d3d7cf 555753 ef2929 8ae234 fce94f 729fcf ad7fa8 34e2e2 eeeeec");
    ("UltraDark",
     "000000 ffffff fefefe 000000 f07178 c3e88d ffcb6b 82aaff c792ea 89ddff cccccc 333333 f6a9ae dbf1ba ffdfa6 b4ccff ddbdf2 b8eaff ffffff");
    ("UltraViolent",
     "242728 c1c1c1 c1c1c1 242728 ff0090 b6ff00 fff727 47e0fb d731ff 0effbb e1e1e1 636667 fb58b4 deff8c ebe087 7fecff e681ff 69fcd3 f9f9f5");
    ("UnderTheSea",
     "011116 ffffff 4afcd6 022026 b2302d 00a941 59819c 459a86 00599d 5d7e19 405555 384451 ff4242 2aea5e 8ed4fd 61d5ba 1298ff 98d028 58fbd6");
    ("Unholy",
     "202020 f8f8f2 f8f8f0 808080 d16969 608b4e fdd877 65aef7 ae81ff 9cdcfe ffffff 808080 d16969 b5cea8 dcdcaa 569cd6 ae81ff 9cdcfe ffffff");
    ("Unikitty",
     "ff8cd9 0b0b0b bafc8b 0c0c0c a80f20 bafc8b eedf4b 145fcd ff36a2 6bd1bc e2d7e1 434343 d91329 d3ffaf ffef50 0075ea fdd5e5 79ecd5 fff3fe");
    ("Urple",
     "1b1b23 877a9b a063eb 000000 b0425b 37a415 ad5c42 564d9b 6c3ca1 808080 87799c 5d3225 ff6388 29e620 f08161 867aed a05eee eaeaea bfa3ff");
    ("Vaughn",
     "25234f dcdccc ff5555 25234f 705050 60b48a dfaf8f 5555ff f08cc3 8cd0d3 709080 709080 dca3a3 60b48a f0dfaf 5555ff ec93d3 93e0e3 ffffff");
    ("VibrantInk",
     "000000 ffffff ffffff 878787 ff6600 ccff04 ffcc00 44b4cc 9933cc 44b4cc f5f5f5 555555 ff0000 00ff00 ffff00 0000ff ff00ff 00ffff e5e5e5");
    ("vimbones",
     "f0f0ca 353535 353535 f0f0ca a8334c 4f6c31 944927 286486 88507d 3b8992 353535 c6c6a3 94253e 3f5a22 803d1c 1d5573 7b3b70 2b747c 5c5c5c");
    ("Violet Dark",
     "1c1d1f 708284 708284 56595c c94c22 85981c b4881d 2e8bce d13a82 32a198 c9c6bd 45484b bd3613 738a04 a57705 2176c7 c61c6f 259286 c9c6bd");
    ("Violet Light",
     "fcf4dc 536870 536870 56595c c94c22 85981c b4881d 2e8bce d13a82 32a198 d3d0c9 45484b bd3613 738a04 a57705 2176c7 c61c6f 259286 c9c6bd");
    ("WarmNeon",
     "404040 afdab6 30ff24 000000 e24346 39b13a dae145 4261c5 f920fb 2abbd4 d0b8a3 fefcfc e97071 9cc090 ddda7a 7b91d6 f674ba 5ed1e5 d8c8bb");
    ("Wez",
     "000000 b3b3b3 53ae71 000000 cc5555 55cc55 cdcd55 5555cc cc55cc 7acaca cccccc 555555 ff5555 55ff55 ffff55 5555ff ff55ff 55ffff ffffff");
    ("Whimsy",
     "29283b b3b0d6 b3b0d6 535178 ef6487 5eca89 fdd877 65aef7 aa7ff0 43c1be ffffff 535178 ef6487 5eca89 fdd877 65aef7 aa7ff0 43c1be ffffff");
    ("WildCherry",
     "1f1726 dafaff dd00ff 000507 d94085 2ab250 ffd16f 883cdc ececec c1b8b7 fff8de 009cc9 da6bac f4dca5 eac066 308cba ae636b ff919d e4838d");
    ("wilmersdorf",
     "282b33 c6c6c6 7ebebd 34373e e06383 7ebebd cccccc a6c1e0 e1c1ee 5b94ab ababab 434750 fa7193 8fd7d6 d1dfff b2cff0 efccfd 69abc5 d3d3d3");
    ("Wombat",
     "171717 dedacf bbbbbb 000000 ff615a b1e969 ebd99c 5da9f6 e86aff 82fff7 dedacf 313131 f58c80 ddf88f eee5b2 a5c7ff ddaaff b7fff9 ffffff");
    ("Wryan",
     "101010 999993 9e9ecb 333333 8c4665 287373 7c7c99 395573 5e468c 31658c 899ca1 3d3d3d bf4d80 53a6a6 9e9ecb 477ab3 7e62b3 6096bf c0c0c0");
    ("zenbones",
     "f0edec 2c363c 2c363c f0edec a8334c 4f6c31 944927 286486 88507d 3b8992 2c363c cfc1ba 94253e 3f5a22 803d1c 1d5573 7b3b70 2b747c 4f5e68");
    ("zenbones_dark",
     "1c1917 b4bdc3 c4cacf 1c1917 de6e7c 819b69 b77e64 6099c0 b279a7 66a5ad b4bdc3 403833 e8838f 8bae68 d68c67 61abda cf86c1 65b8c1 888f94");
    ("zenbones_light",
     "f0edec 2c363c 2c363c f0edec a8334c 4f6c31 944927 286486 88507d 3b8992 2c363c cfc1ba 94253e 3f5a22 803d1c 1d5573 7b3b70 2b747c 4f5e68");
    ("Zenburn",
     "3f3f3f dcdccc 73635a 4d4d4d 705050 60b48a f0dfaf 506070 dc8cc3 8cd0d3 dcdccc 709080 dca3a3 c3bf9f e0cf9f 94bff3 ec93d3 93e0e3 ffffff");
    ("zenburned",
     "404040 f0e4cf f3eadb 404040 e3716e 819b69 b77e64 6099c0 b279a7 66a5ad f0e4cf 625a5b ec8685 8bae68 d68c67 61abda cf86c1 65b8c1 c0ab86");
    ("zenwritten_dark",
     "191919 bbbbbb c9c9c9 191919 de6e7c 819b69 b77e64 6099c0 b279a7 66a5ad bbbbbb 3d3839 e8838f 8bae68 d68c67 61abda cf86c1 65b8c1 8e8e8e");
    ("zenwritten_light",
     "eeeeee 353535 353535 eeeeee a8334c 4f6c31 944927 286486 88507d 3b8992 353535 c6c3c3 94253e 3f5a22 803d1c 1d5573 7b3b70 2b747c 5c5c5c");
    ("Zeonica",
     "01001d ffffff ffffff 0c0c0c cc2929 21c221 d6c315 3e31f5 d918b9 13d4d4 b2b2b2 686868 ff6e6e 6bff6b ffff6b 737cff ff70ff 7fffff ffffff");
  ]
[@@ocamlformat "disable"]

(* WCAG's relative luminance of a colour, and the contrast ratio of two. *)
let luminance c =
  let channel shift =
    let v = float ((c lsr shift) land 0xff) /. 255. in
    if v <= 0.04045 then v /. 12.92 else Float.pow ((v +. 0.055) /. 1.055) 2.4
  in
  (0.2126 *. channel 16) +. (0.7152 *. channel 8) +. (0.0722 *. channel 0)

let contrast a b =
  let la = luminance a and lb = luminance b in
  (Float.max la lb +. 0.05) /. (Float.min la lb +. 0.05)

(* The text under the cursor of a theme without an accent: its background or
   its foreground, whichever stands out more on the cursor. xterm.js draws
   it black, which can vanish on a dark cursor. *)
let accent ~background ~foreground ~cursor =
  if contrast background cursor >= contrast foreground cursor then background
  else foreground

let of_hex s =
  match
    List.map (fun h -> int_of_string ("0x" ^ h)) (String.split_on_char ' ' s)
  with
  | background :: foreground :: cursor :: ansi ->
      {
        background;
        foreground;
        cursor;
        cursor_accent = accent ~background ~foreground ~cursor;
        ansi = Array.of_list ansi;
      }
  | _ -> invalid_arg "Palette.of_hex"

(* VHS takes the first theme of a name. *)
let by_name =
  lazy
    (let h = Hashtbl.create 512 in
     List.iter
       (fun (name, hex) ->
         if not (Hashtbl.mem h name) then Hashtbl.add h name hex)
       themes;
     h)

let find name = Option.map of_hex (Hashtbl.find_opt (Lazy.force by_name) name)
let pp_colour ppf c = Fmt.pf ppf "#%06x" c

let pp ppf t =
  Fmt.pf ppf "@[<h>background %a, foreground %a, cursor %a on %a, ANSI %a@]"
    pp_colour t.background pp_colour t.foreground pp_colour t.cursor pp_colour
    t.cursor_accent
    (Fmt.array ~sep:Fmt.sp pp_colour)
    t.ansi

(* JSON themes *)

exception Colour of string

(* Go matches a key to a field of VHS's [Theme] ignoring case, as
   [bytes.EqualFold] does, where U+017F folds to [s] and U+212A to [k]. *)
let equal_fold key field =
  let b = Buffer.create (String.length key) in
  let rec go i =
    if i < String.length key then (
      let d = String.get_utf_8_uchar key i in
      (match Uchar.to_int (Uchar.utf_decode_uchar d) with
      | 0x17f -> Buffer.add_char b 's'
      | 0x212a -> Buffer.add_char b 'k'
      | _ -> Buffer.add_substring b key i (Uchar.utf_decode_length d));
      go (i + Uchar.utf_decode_length d))
  in
  go 0;
  String.lowercase_ascii (Buffer.contents b) = String.lowercase_ascii field

(* xterm.js's [css.toColor]: [#RGB], [#RGBA], [#RRGGBB], [#RRGGBBAA] and
   [rgb()] or [rgba()]; the alpha is not drawn. *)
let colour s =
  let hex =
    String.length s > 1
    && s.[0] = '#'
    && String.for_all Char.Ascii.is_hex_digit
         (String.sub s 1 (String.length s - 1))
  in
  let digit i = int_of_string ("0x" ^ String.make 1 s.[i]) in
  let byte i = int_of_string ("0x" ^ String.sub s i 2) in
  match String.length s with
  | (4 | 5) when hex ->
      Some
        (((17 * digit 1) lsl 16) lor ((17 * digit 2) lsl 8) lor (17 * digit 3))
  | (7 | 9) when hex -> Some ((byte 1 lsl 16) lor (byte 3 lsl 8) lor byte 5)
  | _ ->
      let re =
        Re.Perl.compile_pat
          {|^rgba?\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*(\d{1,3})\s*(,\s*(0|1|\d?\.(\d+))\s*)?\)|}
      in
      Option.bind (Re.exec_opt re s) (fun g ->
          let c i = int_of_string (Re.Group.get g i) in
          if c 1 > 255 || c 2 > 255 || c 3 > 255 then None
          else Some ((c 1 lsl 16) lor (c 2 lsl 8) lor c 3))

(* xterm.js's colours where a theme has none, or one it cannot read. *)
let xterm =
  {
    background = 0x000000;
    foreground = 0xffffff;
    cursor = 0xffffff;
    cursor_accent = 0x000000;
    ansi =
      [| 0x2e3436; 0xcc0000; 0x4e9a06; 0xc4a000; 0x3465a4; 0x75507b; 0x06989a;
         0xd3d7cf; 0x555753; 0xef2929; 0x8ae234; 0xfce94f; 0x729fcf; 0xad7fa8;
         0x34e2e2; 0xeeeeec |];
  }
[@@ocamlformat "disable"]

(* The fields of VHS's [Theme], all strings: the colours in the order of
   [t], then those xterm.js does not draw here. *)
let fields =
  [ "background"; "foreground"; "cursor"; "cursorAccent"; "black"; "red";
    "green"; "yellow"; "blue"; "magenta"; "cyan"; "white"; "brightBlack";
    "brightRed"; "brightGreen"; "brightYellow"; "brightBlue"; "brightMagenta";
    "brightCyan"; "brightWhite"; "name"; "selection" ]
[@@ocamlformat "disable"]

(* [plain msg] is [msg] without the SGR sequences jsont styles it with. *)
let plain msg =
  let b = Buffer.create (String.length msg) in
  let rec go i =
    if i < String.length msg then
      if msg.[i] = '\027' && i + 1 < String.length msg && msg.[i + 1] = '[' then
        skip (i + 2)
      else (
        Buffer.add_char b msg.[i];
        go (i + 1))
  and skip i =
    if i < String.length msg then
      if msg.[i] = 'm' then go (i + 1) else skip (i + 1)
  in
  go 0;
  Buffer.contents b

(* The string of field [f] in [members], [""] if none: Go keeps the last
   string of a key that matches it, and fails on any other value but
   [null]. *)
let field members f =
  List.fold_left
    (fun acc ((k, _), v) ->
      match (v : Jsont.json) with
      | _ when not (equal_fold k f) -> acc
      | String (s, _) -> Option.map (fun _ -> s) acc
      | Null _ -> acc
      | _ -> None)
    (Some "") members

let theme_of_members members =
  let values = List.map (fun f -> (f, field members f)) fields in
  match List.find_opt (fun (_, v) -> v = None) values with
  | Some (f, _) -> Error (`Json (Fmt.str "the %s of a theme is a string" f))
  | None -> (
      let get i default =
        match List.nth values i with
        | _, Some "" -> default
        | f, Some c -> (
            match colour c with
            | Some c -> c
            | None -> raise_notrace (Colour (f ^ " " ^ c)))
        | _, None -> default
      in
      match
        let background = get 0 xterm.background
        and foreground = get 1 xterm.foreground
        and cursor = get 2 xterm.cursor in
        {
          background;
          foreground;
          cursor;
          cursor_accent = get 3 (accent ~background ~foreground ~cursor);
          ansi = Array.init 16 (fun i -> get (4 + i) xterm.ansi.(i));
        }
      with
      | t -> Ok t
      | exception Colour c -> Error (`Colour c))

(* Go's encoding/json refuses arrays and objects nested deeper than this,
   the outermost one included. *)
let max_depth = 10_000

(* [too_deep s] is [true] iff [s] opens more than [max_depth] arrays and
   objects at once, outside its strings. *)
let too_deep s =
  let n = String.length s in
  let rec go i depth ~quoted =
    i < n
    &&
    match s.[i] with
    | '\\' when quoted -> go (i + 2) depth ~quoted
    | '"' -> go (i + 1) depth ~quoted:(not quoted)
    | ('[' | '{') when not quoted ->
        depth >= max_depth || go (i + 1) (depth + 1) ~quoted
    | (']' | '}') when not quoted -> go (i + 1) (depth - 1) ~quoted
    | _ -> go (i + 1) depth ~quoted
  in
  go 0 0 ~quoted:false

let of_json s =
  if too_deep s then
    Error (`Json (Fmt.str "nested deeper than %d levels" max_depth))
  else
    match Jsont_bytesrw.decode_string' ~locs:true Jsont.json s with
    | Ok (Object (members, _)) -> theme_of_members members
    | Ok _ -> Error (`Json "a theme is a JSON object")
    | Error (_, meta, kind) ->
        let at = Jsont.Textloc.first_byte (Jsont.Meta.textloc meta) in
        let msg =
          Fmt.str "%s at offset %d" (plain (Jsont.Error.kind_to_string kind)) at
        in
        (* Go reads invalid UTF-8 and an unpaired surrogate escape as
           U+FFFD, where jsont stops. *)
        let surrogate =
          let n = String.length msg and k = String.length "surrogate" in
          let rec go i =
            i + k <= n && (String.sub msg i k = "surrogate" || go (i + 1))
          in
          go 0
        in
        if surrogate || not (String.is_valid_utf_8 s) then Error (`Unread msg)
        else Error (`Json msg)
