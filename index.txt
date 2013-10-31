                        A.I. Freecell Solver                         
                                                                     
---------------------------------------------------------------------

For CMSC671 (Graduate level A.I) we were required to "design,
implement, demonstrate, evaluate, and report on a working AI system
in Lisp that combines significant components of the course." Our
group decided to implement a freecell solver. I ended up writing
almost all the code and my partner ended up writing most of the
report. Because our solver is rather unique I decided to release the
code under the GPL. In general, our solutions are shorter than those
found by a comparable Free Cell solver found at http://
vipe.technion.ac.il/~shlomif/freecell-solver, although the a-star
search found in that package seems to do better with some of the most
difficult games. It should work with any ANSI Common List
implementation; however because the search is computationally intense
using clisp or some other interpretative lisp implantation is not
recommend unless you enjoy waiting for ever. Of the two compiled Lisp
implementations tried, Allegro and CMUCL, CMUCL gives the best
performance. The final code has only been tested with CMUCL.

  * Final Report (pdf): Part 1, Part 2
  * Browse Code Online
  * Download: tar.gz, zip.
  * Freecell FAQ (offsite)

---------------------------------------------------------------------
This page is maintained by Kevin Atkinson (kevin.fc at atkinson dhs
org), to my Home Page
