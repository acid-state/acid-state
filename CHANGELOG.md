0.16.1.2
========

- Support unix-2.8
- Tested with GHC 7.8 - 9.6.0

0.16.1.1
========

- Adapt to changes in hedgehog-1.1 related to barbies
- Support mtl-2.3
- Tested with GHC 7.8 - 9.2.3

0.16.1
======

 - Support GHC-9.0.2, template-haskell 2.18

0.16.0
======

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
