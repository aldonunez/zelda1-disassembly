Introduction
------------

Years ago, I reverse engineered The Legend of Zelda ad hoc for the purpose of remaking it in
a high-level language. I remade it as complete as I could. But, I always felt that the
disassembly was a bit wasted, because - despite being necessary for the remake - was incomplete,
poorly documented, and generally unfit for sharing.

The desire to start this complete disassembly project came after taking inspiration from
the work of Disch on the Final Fantasy disassembly, and Doppelganger's Super Mario Bros.
disassembly.


Development
-----------

I started this project in October 2020. Although I used and enjoyed FCEUX for the original
ad hoc reverse engineering project in 2013; I found the Mesen emulator to be more helpful this
time around, because of several features, including:

- adding and changing labels on the fly
- adding comments
- syntax coloring
- the event viewer (useful in figuring out scrolling)

The disassembly listing in this project was produced by a tool I wrote that processes Mesen's
.MLB file and the ROM. Among other features, it figures out:

- how to generate ca65 cheap and unnamed labels
- what procedures are mapped from different banks at each call site
- replaces memory instruction operands with custom expressions
- disassembles machine code


Building a ROM
--------------

The source files can be assembled with ca65. To build a ROM, first make an ext folder next to
src and put ca65 and ld65 inside. Then run build.ps1. The output will appear in a bin folder.

The build script expects to find Original.nes in the ext folder to compare the output to.
Original.nes must be the "(U) (PRG0) [!]" version of the ROM.

To skip the verification, pass the option --NoVerify to build.ps1.


Contact
-------

Aldo Núñez

aldonunez1@gmail.com
