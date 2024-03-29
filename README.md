# Free ListT

This package provides a lawful `ListT` implementation based on free monads.
It also provides an _Applicative_ list transformer, which is a lawful `Applicative`,
and isomorphic to the old `ListT`.

## Background

The old `ListT` transformer from [`transformers < 0.6`](https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-List.html) was unlawful:
For a noncommutative monad, `ListT m` was not a monad.
It was implemented simply as the composition of list and `m`:

```haskell
newtype ListT m a = ListT (m [a])
```

As such, it is automatically a lawful `Functor` and `Applicative` in a canonical way.
But famously, a composition of two monads is _not_ always a monad, and in this case, it in fact isn't.

### Previous approaches

#### Streaming

There is one popular approach to `ListT`, representing the transformer as a stream:

```haskell
newtype ListT m a = ListT (m (Maybe (a, ListT m a)))
```

This approach is implemented in https://hackage.haskell.org/package/list-t and https://hackage.haskell.org/package/list-transformer,
and it is a great choice in many cases.

Basically, to go through the list, one has to perform one effect in `m` at each step,
and one discovers whether the list now ends or produces a further element.
But this also means that the list structure enforces all earlier `m` effects before a later element can be accessed.

#### Church-encoding

Another possibility is Church-encoding the list, which is implemented in [`logict`](https://hackage.haskell.org/package/logict).

### Free approach

A simple algebraic approach that does not enforce the linear structure of streaming `ListT` is a free monad:

```haskell
newtype ListT m a = ListT (FreeT [] m a)
```
This gives a branching rose tree of computations, where at every step in `m`, arbitrarily many branches can arise.

Unfolding the definition of `FreeT` as an algebraic datatype would result in something like:

```haskell
data ListT m a = OneLayer (m a) | TwoLayers (m [m a]) | ThreeLayers (m [m [m a]]) | ...
```

### Applicative transformer

As mentioned already, the old `ListT` is a valid Applicative transformer,
which means that `ListT m` is a lawful `Applicative` as long as `m` is.
This is useful and sufficient in many situations,
which is why it is reinstated here.

Also, values of type `m [a]` occur very often in the wild
(for example when using [`mapM`](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Base.html#v:mapM)),
so it makes sense to give them a proper type that captures their properties.

To give some more examples, all lawful `ListT` implementations have some kind of "running" function
that maps it to `m [a]`.
The free `ListT` is no exception here.
In a sense, one can "flatten" or "concatenate" all free list layers into a single one.
While this is a natural transformation, this is of course not a monad morphism.
