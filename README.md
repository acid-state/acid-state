acid-state [![Hackage version](https://img.shields.io/hackage/v/acid-state.svg?label=Hackage&color=informational)](http://hackage.haskell.org/package/acid-state) [![acid-state on Stackage Nightly](https://stackage.org/package/acid-state/badge/nightly)](https://stackage.org/nightly/package/acid-state) [![Stackage LTS version](https://www.stackage.org/package/acid-state/badge/lts?label=Stackage)](https://www.stackage.org/package/acid-state) [![Cabal build](https://github.com/acid-state/acid-state/workflows/Haskell-CI/badge.svg)](https://github.com/acid-state/acid-state/actions)
==========

Unplug your machine, restart and have your app recover to exactly where it left off. Acid-State spares you the need to deal with all the marshalling, consistency, and configuration headache that you would have if you used an external DBMS for this purpose.

How does it work?
===========
Acid-state does not write your data types to disk every time you change it. It instead keeps a history of all the functions (along with their arguments) that have modified the state. Thus, recreating the state after an unforeseen error is a simple as rerunning the functions in the history log.

Keep in mind that acid-state does not provide schema migrations.
If you plan on changing the definition of your data-type
during the lifetime of your application (you most likely do),
you can either use a fixed schema such as `XML` or `JSON`,
or you can use [`safecopy`](https://github.com/acid-state/safecopy "safecopy").
As of version 0.4, `safecopy` is the default serialization path
but using `XML` or `JSON` is still a possibility.

FAQ
============
- Will my data still be accessible with future versions of AcidState?
  - Yes, all future versions will be compatible with, or easily upgradeable from, older versions.
- Is AcidState thread-safe?
  - Yes, using AcidState from multiple threads will only increase performance.
- Does AcidState work on Windows?
  - Yes, as of version 0.5.1, acid-state works on Windows.
- Is using Template Haskell (makeAcidic) recommended?
  - Yes.
- Is it necessary to use Template Haskell?
  - No, all instances and data-types can be declared by hand. See the NoTH examples
- Can two processes access the same AcidState store?
  - Using `Data.Acid.Remote` you can create a UnixSocket and one process can communicate with another though a ‘file’ on the file system. Check out examples/Proxy.hs in the github source for acid-state.
- Can two machines access the same AcidState store?
  - `Data.Acid.Remote` can do that as well. See RemoteClient / RemoteServer. At the moment, zero security is provided. But that is quite fixable.
- Does AcidState have a mechanism for interactive queries?
  - Using `Data.Acid.Remote`, you should be able to fire up `ghci` and run existing queries interactively. You can then munge that data using normal Haskell functions. Obviously, this is not a total solution. It would be nice to see some generics based stuff added to make this process friendlier and more powerful.
- My process seems to be consuming a bunch of CPU even though it is idle, why is that?
  - Run your application with `+RTS -I0` (which can be done via compile time options in your .cabal). This disables idle-time garbage collection which is almost always the cause of CPU usage while idle in acid-state based apps.
- How does AcidState deal with the presence of very big data structures which may not fit in memory?
  - There are a few answers to this depending on the situation. One solution is to just buy more RAM. After all, you can buy machines with 1TB of RAM these days. But, obviously, not everyone has that kind of budget. (Interestingly, Facebook keeps something like 90% of their working data in RAM using TBs of memcached servers.)

  - A related solution is to buy more RAM, but spread it across multiple machines using replication/sharding. That area of acid-state is still in development. happstack-state has had a few experimental replication implementations. Lemmih is working on a new approach for acid-state.

  - Another potential solution is to create a special data-structure like IxSet which stores only the keys in RAM, and uses mmap and/or enumerators to transparently read the values from disk without loading them all into RAM at once.

  - You could also do something similar but in a more manual/controlled way where you store parts of your data structure in acid-state, and use another system to store key/value blobs on the disk which you load in explicitly. Allow your app to have very fine control over what, when, and how data gets loads into RAM, flushed to disk, etc. Rather than having a general purpose system try to guess what data should be in RAM.

  - It’s a cost vs speed tradeoff. The only reason anyone would choose to use disk over RAM is because it is cheaper. But it is also 100x slower (or more). Obviously, in many cases much slower is still fast enough. However, acid-state still aims to beat traditional SQL in terms of development and maintenance effort.

Robustness
================
  - How do I recover a corrupted acid-state database?
    - acid-state provides an `acid-state-repair` tool. Build it with `stack build` in the root of this repository, you can then call `acid-state-repair` from a directory holding a corrupted database to repair it. In most case it should only drop the last entry, presumably a partial write because of a crash, but if the base is heavily corrupted, it may restore the database to an older state. Your database is always saved in `.bak` files, so that the repair operation is reversible.
  - How well does acid-state deal with errors?
    - List of Error Scenarios.
