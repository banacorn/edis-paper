%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Type-level Functions}
\label{sec:type-level-fun}

\subsection{Closed Type Families}

Type families have a wide variety of applications. They can appear inside type
 classes\cite{tfclass,tfsynonym}, or at toplevel. Toplevel type families
 can be used to compute over types, they come in two forms: open\cite{tfopen}
 and closed \cite{tfclosed}.

We choose \emph{closed type families}, because it allows overlapping instances,
 and we need none of the extensibility provided by open type families.
For example, consider both term-level and type-level |&&|:

\begin{spec}
(&&) :: Bool -> Bool -> Bool
True && True = True
a    && b    = False

type family And (a :: Bool) (b :: Bool) :: Bool where
    And True True = True
    And a    b    = False
\end{spec}

The first instance of |And| could be subsumed under a more
 general instance, |And a b|.
But the closedness allows these instances to be resolved in order, just like
 how cases are resolved in term-level functions. Also notice that how much
 |And| resembles to it's term-level sibling.

\subsection{Functions on Type-Level Dictionaries}

With closed type families, we could define functions on the type level.
 Let's begin with dictionary lookup.

\begin{spec}
type family Get
    (xs :: [(Symbol, *)])   -- dictionary
    (s :: Symbol)           -- key
    :: * where              -- type

    Get (’(s, x) ’: xs) s = x
    Get (’(t, x) ’: xs) s = Get xs s
\end{spec}

Another benefit of closed type families is that type-level equality can be
expressed by unifying type variables with the same name.
|Get| takes two type arguments, a dictionary and a symbol.
If the key we are looking for unifies with the symbol of an entry, then
 |Get| returns the corresponding type, else it keeps
 searching down the rest of the dictionary.

|Get OpenTList OpenTPar "A", Int CLoseTPar CloseTList "A"| evaluates to
|Int|.

But |Get OpenTList OpenTPar "A", Int CLoseTPar CloseTList "B"| would get stuck.
That's because |Get| is a partial function on types,
 and these types are computed at compile-time. It wouldn't make
 much sense for a type checker to crash and throw a ``Non-exhaustive'' error or
 be non-terminating.

We could make |Get| total, as we would at the term level,
 with |Maybe|.

\begin{spec}
type family Get
    (xs :: [(Symbol, *)])   -- dictionary
    (s :: Symbol)           -- key
    :: Maybe * where        -- type

    Get ’[]             s = Nothing
    Get (’(s, x) ’: xs) s = Just x
    Get (’(t, x) ’: xs) s = Get xs s
\end{spec}
%
Other dictionary-related functions are defined in a similar fashion.

\begin{spec}
-- inserts or updates an entry
type family Set
    (xs :: [(Symbol, *)])   -- old dictionary
    (s :: Symbol)           -- key
    (x :: *)                -- type
    :: [(Symbol, *)] where  -- new dictionary

    Set ’[]             s x = ’[ ’(s, x) ]
    Set (’(s, y) ’: xs) s x = (’(s, x) ’: xs)
    Set (’(t, y) ’: xs) s x =
        ’(t, y) ’: (Set xs s x)

-- removes an entry
type family Del
    (xs :: [(Symbol, *)])   -- old dictionary
    (s :: Symbol)           -- key
    :: [(Symbol, *)] where  -- new dictionary

    Del ’[] s             = ’[]
    Del (’(s, y) ’: xs) s = xs
    Del (’(t, y) ’: xs) s = ’(t, y) ’: (Del xs s)

-- membership
type family Member
    (xs :: [(Symbol, *)])   -- dictionary
    (s :: Symbol)           -- key
    :: Bool where           -- exists?

    Member ’[]             s = False
    Member (’(s, x) ’: xs) s = True
    Member (’(t, x) ’: xs) s = Member xs s
\end{spec}

\subsection{Proxies and Singleton Types}

Now we could annotate the effects of a command in types. \text{DEL}
 removes a key from the current database, regardless of its type.

\begin{spec}
del :: KnownSymbol s
    => Proxy s
    -> Popcorn xs (Del xs s) (Either Reply Integer)
del key = Popcorn $ Hedis.del (encodeKey key)
\end{spec}

|KnownSymbol| is a class that gives the string associated
 with a concrete type-level symbol, which can be retrieved with
 |symbolVal|.\footnotemark
 Where |encodeKey| converts |Proxy s| to
 |ByteString|.
\footnotetext{They are defined in |GHC.TypeLits|.}

\begin{spec}
encodeKey :: KnownSymbol s => Proxy s -> ByteString
encodeKey = encode . symbolVal
\end{spec}

Since Haskell has a \emph{phase distinction, phasedistinction}, types are
 erased before runtime. It's impossible to obtain information directly from
 types, we can only do this indirectly, with
 \emph{singleton types, singletons}.

A singleton type is a type that has only one instance, and the instance can be
 think of as the representative of the type at the realm of runtime values.

|Proxy|, as its name would suggest, can be used as
 singletons. It's a phantom type that could be indexed with any type.

\begin{spec}
data Proxy t = Proxy
\end{spec}

In the type of |del|, the type variable
 |s| is a |Symbol| that is decided by
 the argument of type |Proxy s|.
 To use |del|, we would have to apply it with a clumsy
 term-level proxy like this:

\begin{spec}
del (Proxy :: Proxy "A")
\end{spec}
