

# efranc : Detect the language of text. #

Copyright (c) 2017 G-Corp

__Version:__ 0.0.1

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@gmail.com`](mailto:gregoire.lejeune@gmail.com)).


### Api ###


#### DATA TYPES ####

`options() = #{whitelist => whitelist(), blacklist => blacklist(), min_length => integer()}`

`whitelist() = [lang()]`

`blacklist() = [lang()]`


#### EXPORTS ####

<h5>`efranc:detect(Text, Options) -> Lang`</h5>

Types:

__`Text = string()`<br />`Options = options()`<br />`Lang = lang()`__

`detect/2` return the detected language for the `Text` or `undefined`.

<h5>`efranc:detect(Text) -> Lang`</h5>

Equivalent to `efranc:detect(Text, #{})`

<h5>`efranc:detect_all(Text, Options) -> [{Lang, Weight}]`</h5>

Types:

__`Text = string()`<br />`Options = options()`<br />`Lang = lang()`<br />`Weight = float()`__

`detect_all/2` return the list of possible detected language for the `Text` or `undefined`.

<h5>`efranc:detect_all(Text) -> [{Lang, Weight}]`</h5>

Equivalent to `efranc:detect_all(Text, #{})`


### Command line tool ###

Build:

```

make script

```

Use :

```

./_build/escriptize/bin/efranc -h
Usage: efranc [-w <whitelist>] [-b <blacklist>] [-m <min_size>] [-h]

  -w, --whitelist  Whitelist
  -b, --blacklist  Blacklist
  -m, --min_size   Minimum size
  -h, --help       Display this help

```

Example :

```

./_build/escriptize/bin/efranc "Bonjour tout le monde, ceci est un example d'utilisation d'efranc en ligne de commande ! Enjoy ;)"
fra | Bonjour tout le monde, ceci est un example d'utilisation d'efranc en ligne de...

```


### Derivation ###

eFranc is a port to Elang of [franc](https://github.com/wooorm/franc).


### Licence ###

efranc is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2017 G-Corp<br />

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.



THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/G-Corp/efranc/blob/master/doc/efranc.md" class="module">efranc</a></td></tr></table>

