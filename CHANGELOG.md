0.16.1.3
========4
_Henry Laxen, 2025-03-04_

- Fixed code to remove all warnings
- Tested with GHC 9.10.1

0.16.1.3
========

_Andreas Abel, 2023-08-03_

- Support template-haskell-2.21
  ([#162](https://github.com/acid-state/acid-state/pull/162))
- Tested with GHC 7.8 - 9.8.1-alpha1

0.16.1.2
========

_Andreas Abel, 2023-04-06_

- Support unix-2.8
- Tested with GHC 7.8 - 9.6.1

0.16.1.1
========

_Andreas Abel, 2022-06-01_

- Adapt to changes in hedgehog-1.1 related to barbies
- Support mtl-2.3
- Tested with GHC 7.8 - 9.2.3

0.16.1
======

_David Fox, 2022-02-18_

 - Support GHC-9.0.2, template-haskell 2.18

0.16.0.1
========

_Jeremy Shaw, 2020-05-19_

0.16.0
======

_Jeremy Shaw, 2019-11-14_

 - support network-3.x and ghc-8.8.1
 - Fix tests
   ([#131](https://github.com/acid-state/acid-state/pull/131))
 - Haddock documentation fixes
 - Update build dependency constraints

0.15.2
======

 - adds acid-state-repair recovery tool
   ([#126](https://github.com/acid-state/acid-state/pull/16))
 - parameterize the underlying serialization library
   ([#96](https://github.com/acid-state/acid-state/pull/96))
 - support safecopy-0.10, which supports GHC.Generics
   ([#128](https://github.com/acid-state/acid-state/pull/128))

0.15.0
======

 - change text of error messages to include module names
   ([#116](https://github.com/acid-state/acid-state/pull/116))
 - depend on filelock library to avoid locking bug
   ([#91](https://github.com/acid-state/acid-state/pull/91))
 - permit events that are polymorphic in the base monad, with a MonadReader/MonadState constraint
   ([#94](https://github.com/acid-state/acid-state/pull/94))
 - fix a minor memory leak
   ([#104](https://github.com/acid-state/acid-state/pull/104))
 - add a test suite and extend examples
   ([#98](https://github.com/acid-state/acid-state/pull/98))
 - improve benchmarks
   ([#113](https://github.com/acid-state/acid-state/pull/113))
 - expose internal modules (subject to change in the future)

0.14.3
======

 - support building on GHC 8.2
 - update links from seize.it to github.com

0.14.2
======

 - createCheckpoint now cuts a new events file
   ([#74](https://github.com/acid-state/acid-state/pull/74))

0.14.1
======

 - fix bug in archiveLog that resulted in logs being moved prematurely
   ([#22](https://github.com/acid-state/acid-state/issues/22))
 - tweaks for GHC 8.0.x, template-haskell 2.11.x
 - fix compilation of benchmarks

0.14.0
======

 - fixes for cereal 0.5 while maintaining cereal 0.4
   compatibility. IMPORTANT: cereal 0.5 / safecopy 0.9 change the
   serialization format of Float/Double. Migration should be performed
   automatically on old data. However, you should be aware that once
   you upgrade to safecopy 0.9 / cereal 0.5, your data will be
   migrated and not readable by older versions of your application
   which are compiled against safecopy 0.8 / cereal 0.4.

 - additional fixes for TH and kinded type variables
   ([#56](https://github.com/acid-state/acid-state/pull/56))
