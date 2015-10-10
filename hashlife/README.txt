                               HASHLIFE
                               ========

                           By Robert Smith


This directory contains an implementation of Bill Gosper's HASHLIFE
algorithm.


Notes on Implementation
-----------------------

* The code is not portable; it is SBCL specific. In particular, it
  uses:

      - SB-EXT:MIXF for hash code mixing

      - SB-EXT:DEFGLOBAL for non-special, fast-access global variables

      - SB-EXT:DEFINE-HASH-TABLE-TEST for telling MAKE-HASH-TABLE
        about our custom hash function and test function
      
      - SBCL's weak extensions to MAKE-HASH-TABLE

  In principle, all of these things exist in most other
  implementations of Common Lisp, so porting should be easy.

* The macrocells have 2x2 "leaf" nodes. There is no good reason for
  this except that every node has children of some sort, which allows
  hashing. (Obviously, this somewhat contracts the very definition of
  a "leaf node", hence the quotation marks.)

* Hashing is a recursive function of the quadtree children. Hash
  values are mixed using SBCL's MIX(F) function.

* Caching is done with a weak hash table. The hash table is used to
  implement a set data structure. Querying the hash table requires
  allocation of a "query macrocell", which is just used as a proxy for
  hashing. This can be eliminated if we implement hash tables
  manually.

* As a result of the previous point, the table is garbage collected
  when the Lisp system garbage collects.

* Using bitwise arithmetic for computing neighbor counts was inspired
  by Tomas Rokicki, as was the internal variable naming conventions
  within TIMESTEP and HYPERSTEP.


CHARMLIFE Viewer
----------------

CHARMLIFE is a very simple viewer for the Game of Life universe using
the CL-CHARMS library.

To start, load the CHARMLIFE system and run #'CHARMLIFE:MAIN.

Keys:

    w a s d : Move up, left, down, right.
    SPACE   : Toggle cell on or off.
    q or Q  : Quit.
    !       : Run HASHLIFE on the universe.
