[school assignment](http://perso.ens-lyon.fr/daniel.hirschkoff/P2/docs/rendu1.pdf)

## Usage
- compile with `make`
- run with `./f2bdd blabla.form`

## ROBDD & Hash-consing

After a bit of research on hashconsing (what we want to enforce on our bdds), we found [a paper by Filliâtre][hashconsing] explaining how to write a proper implementation in OCaml.
Hash-consing done in Filliâtre's way makes many things a lot easier: we have the guarantee that values of the type `hash_consed` are in fact hash-consed, allowing the use of *physical equality* instead of *structural equality* (expected speedup).
A nice feature of Filliâtre's implementation is that it plays alongside garbage collection, meaning if objects can only be found in the internal table used during the building process, they can be collected by garbage collection (programs should in turn be more memory efficient, as opposed to using a simple Hashmap).
Finally, each `hash_consed` term is associated with a unique integer id (its `tag`), allowing for the use of somewhat efficient data structures.  
*In our code, to apply a process to `hash_consed` objects only once, we create a `Set` of objects in which they are ordered by their tag.*

### Notable differences

- Instead of having a reference to some global integer for the next tag available, used by every `hash_consed` data structure, we restricted such an integer to the `hash_consed` data structure to not waste tags as much. It also means two `hash_consed` terms created from different data structures could have the same `tag`, but we did not find any situation in which it may have been an issue.

- Filliâtre's implementation allows for a flexible resize mechanism, in which you can specify a treshold (of the mean number of elements stored per bucket of the underlying hashmap) beyond which the table should be resized. We opted to fix this threshold to 3, meaning if there is an average of more than 3 items per bucket, a resize operation is triggered.

- Regarding resizing, the original implementation relies on mutually recursives functions (`add` and `resize`) meaning in theory if you're not careful a resize operation could trigger another resize operation and so on. This is where they fiddle with the threshold mentionned earlier, to prevent such an event from happening.  
  We decided to make these functions independant, and make `hashcons` be the function to trigger `resize` when needed.

[hashconsing]: https://www.lri.fr/~filliatr/ftp/publis/hash-consing2.pdf
