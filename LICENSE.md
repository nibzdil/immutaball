# Licensing and exceptions

Immutaball is licensed under the terms of the GNU General Public License
version 3 or later (at your option), since it is a derivative work of Neverball
1.7.0-alpha.3, which is released under the terms of the GNU General Public
License version 2 or a later version (at your option), and links with its data
files.  The full texts of GPLv3 and GPLv2 are included in `doc/legal/`.

Immutaball core, under `src/`, is a port of Neverball core to Haskell.  The
separable components of Immutaball core independent from Neverball core are
licensed under the terms of 0BSD, the text of which is included in
`doc/legal/`.  Consent here is thus also provided under the terms of 0BSD as
well.  (The combined project, as stated, is licensed under GPL-3.0-or-later to
be compliant with the usage terms of the dependencies it uses.)

Immutaball uses a number of external libraries.  Certain features that can be
enabled at compile time require additional libraries.  Binary versions of these
libraries are sometimes distributed with Immutaball. Licensing information for
these libraries is included in `doc/legal/`.

Immutaball includes a number of third party fonts. The DejaVu font
(`submodules/neverball/data/ttf/DejaVuSans-Bold.ttf`) license is included in
`doc/legal/`. The WenQuanYi Micro Hei font
(`submodules/neverball/data/ttf/wqy-microhei.ttc`) is dual licensed under
Apache 2.0 or GPLv3. The full text of GPLv3 is included in `doc/legal/`.

Neverball uses the Octocat ball `submodules/neverball/data/ball/octocat` with
the Octocat design with permission permission from Github dated 2013-11-12.
Wording of approval is in `doc/legal`.

The source file `submodules/neverball/share/fs_jpg.c` is based on `jdatasrc.c`
from the libjpeg project.

The source files `submodules/neverball/share/miniz.[ch]` are included verbatim
from the miniz project (https://github.com/richgel999/miniz) and are licensed
under the MIT license. See `doc/legal/license-miniz.txt`.
