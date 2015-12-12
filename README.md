pred-trie
=========

A predicative trie library - use predicates instead of literal match to capture
classes of results, instead of enumerating distinguished ones.

## Usage

The predicates are existentially quantified such that a predicate _creates_ an
unknown type, while it's result must have the _necessary arity_, matching the
quantified type, to fulfill the lookup:

```haskell
PredTrie s a
  = PNil
  | forall t. PCons
      { predicate :: s -> Maybe t
      , result    :: t -> a
      }
```

...basically.

I broke the lookup phases into "steps", like the [tries](https://github.com/athanclark/tries)
package, and used the fastest-lookup `HashMapStep` trie implementation for the
literal lookups. For more info, read the code :D

## How to run tests

```bash
stack test
```

## Benchmarking

```bash
stack bench --benchmark-arguments="--output profile.html"
```
