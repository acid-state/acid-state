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
   [https://github.com/acid-state/acid-state/pull/56](https://github.com/acid-state/acid-state/pull/56)
