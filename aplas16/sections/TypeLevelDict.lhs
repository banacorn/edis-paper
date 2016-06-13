%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Type-Level Dictionaries}
\label{sec:type-level-dict}

One of the challenges of statically ensuring type correctness of \Redis{},
which also presents in other stateful languages, is that the type of the value
associated to a key can be altered after updating. To ensure type correctness,
we keep track of the types of all existing keys in a {\em dictionary} ---
conceptually, an associate list, or a list of pairs of keys and \Redis{} types.
For example, the dictionary |[("A",Int), ("B", Char), ("C", Bool)]| represents
a predicate stating that ``the keys |"A"|, |"B"|, and |"C"| are respectively
associated to values of type |Int|, |Char|, and |Bool|.''

The dictionary mixes values (strings such as |"A"|, |"B"|) and types. Further
more, as mentioned in Section~\ref{sec:indexed-monads}, the dictionaries are
to be used parameters to the indexed monad |Edis|. In a dependently typed
programming language (without the so-called ``phase distinction'' ---
separation between types and terms), this would pose no problem. In Haskell
however, the dictionaries, to index a monad, has to be a type as well.

In this section we describe how to construct a type-level dictionary, to be
used with the indexed monad in Section~\ref{sec:indexed-monads}.

\subsection{Datatype Promotion}

Haskell maintains the distinction between values, types, and kinds: values are
categorized by types, and types are categorized by kinds. The kinds are
relatively simple: |*| is the kind of all {\em lifted} types, while type
constructors have kinds such as |* -> *|, |* -> * -> *|, etc. Consider the
datatype definitions below:
\begin{spec}
data Nat = Zero | Suc Nat {-"~~,\qquad"-} data [a] = [] | a : [a] {-"~~."-}
\end{spec}
The lefthand side is usually seen as having defined a type |Nat :: *|,
and two value constructors |Zero :: Nat| and |Suc :: Nat -> Nat|. The righthand
side is how Haskell lists are understood. The {\em kind} of |[.]| is |* -> *|,
since it takes a lifted type |a| to a lifted type |[a]|. The two value
constructors respectively have types |[] :: [a]| and |(:) :: a -> [a] ->
[a]|, for all type |a|.

The GHC extension \emph{data kinds}~\cite{promotion}, however, automatically
promotes certain ``suitable'' types to kinds.\footnote{It is only informally
described in the GHC manual what types are ``suitable''.} With the extension,
the |data| definitions above has an alternative reading: |Nat| is a new kind,
|Zero :: Nat| is a type having kind |Nat|, and |Suc :: Nat -> Nat| is a type
constructor, taking a type in kind |Nat| to another type in |Nat|. Whether a
constructor is promoted can often be inferred from the context. To be more
specific, prefixing a constructor with a single quote, such as in |ZERO| and
|SUC|, denotes that it is promoted.

The situation of lists is similar: for all kind |k|, |[k]| is also a kind. For
all kind |k|, |[] :: [k]| is a type. Given a type |x|
of kind |k| and a type |xs| of kind |[k]|, |x : xs| is again a type of
kind |[k]|. Formally, |(:) :: k -> [k] -> [k]|. For example,
|Int :- (Char :- (Bool :- NIL))| is a type having kind |[*]| --- a list of
(lifted) types. The optional quote denotes that the constructors are promoted.
The same list can be denoted by a syntax sugar |TList (Int, Char, Bool)|.

Tuples are also promoted. Thus we may put two types in a pair to form another
type, such as in |TPar (Int, Char)|, a type having kind |(*,*)|.

Strings in Haskell are nothing but lists of |Char|s. Regarding promotion,
however, a |String| can be promoted to a type having kind |Symbol|. |Symbol| is
a type without a constructor: |data Symbol|,
intended to be used as a promoted kind. In the expression:
\begin{spec}
"this is a type-level string literal" :: Symbol {-"~~,"-}
\end{spec}
the string on the lefthand side of |(::)| is a type, having kind |Symbol|.

With all of these ingredients, we are ready to build our dictionaries, or
type-level associate lists:
\begin{spec}
type DictEmpty = NIL {-"~~,"-}
type Dict0 = TList (TPar ("key", Bool)) {-"~~,"-}
type Dict1 = TList (TPar ("A", Int), TPar ("B", "A")) {-"~~."-}
\end{spec}
All the entities defined above are types, where |Dict0| and |Dict1|
have kind |[(Symbol, *)]|.

\subsection{Type-Level Functions}

Now that we can represent dictionaries as types, the next step is to define
operations on them. A function that inserts an entry to a dictionary, for
example, is a function from a type to a type. While it was shown that it is
possible to simulate type-level functions using Haskell type
classes~\cite{McBride:02:Faking}, in recent versions of GHC, {\em indexed type
families}, or type families for short, are considered a cleaner solution.

For example, compare conjunction |(&&)| and its type-level
counterpart |And|:\\
\noindent{\centering %\small
\begin{minipage}[b]{0.35\linewidth}
\begin{spec}
(&&) :: Bool -> Bool -> Bool
True  &&  True  = True
a     &&  b     = False {-"~~,"-}
\end{spec}
\end{minipage}
\begin{minipage}[b]{0.55\linewidth}
\begin{spec}
type family And (a :: Bool) (b :: Bool) :: Bool
  where  And  True  True  = True
         And  a     b     = False {-"~~."-}
\end{spec}
\end{minipage}
}\\
The lefthand side is a typical definition of |(&&)| by pattern matching.
On the righthand side, |Bool| is not a type, but a type lifted to a kind,
while |True| and |False| are types of kind |Bool|. The declaration says
that |And| is a family of types, indexed by two parameters |a| and |b| of
kind |Bool|. The type with index |True| and |True| is |True|, and all
other indices lead to |False|. For our purpose, we can read |And| as a
type-level function. Observe how it resembles the term-level |(&&)|.

Note that type families in Haskell come in many flavors. Families can be
defined for |data| and |type| synonym. They can appear inside type
classes~\cite{tfclass,tfsynonym} or at toplevel. Toplevel type families
can be open~\cite{tfopen} or closed~\cite{tfclosed}. The flavor we chose
is top-level, closed type synonym family, since it allows overlapping
instances, and we need none of the extensibility provided by open type
families. Notice that the instance |And True True| could be subsumed under
the more general instance, |And a b|. In a closed type family we may resolve
the overlapping in order, just like how cases overlapping is resolved in
term-level functions.

%\subsection{Functions on Type-Level Dictionaries}

We are now able to define operations on type-level dictionaries.
Let's begin with dictionary lookup.
\begin{spec}
type family Get (xs :: [(Symbol, *)]) (s :: Symbol) :: * where
    Get (TPar (s, x) :- xs) s  =  x
    Get (TPar (t, x) :- xs) s  =  Get xs s {-"~~"-}
\end{spec}
The type-level function |Get| returns the entry associated with key |s| in the
dictionary |xs|. Notice, in the first case, how type-level equality can be
expressed by unifying type variables with the same name.
Note also that |Get| is a partial function on types:
|Get (TList (TPar ("A", Int))) "A"| evaluates to |Int|, but
|Get (TList (TPar ("A", Int))) "B"| gets stuck.
... and these types are computed at compile-time. It wouldn't make
much sense for a type checker to crash and throw a ``Non-exhaustive'' error or
be non-terminating.
\todo{So, what exactly happens when we do the second?}

We could make |Get| total, as we would at the term level, with |Maybe|:
\begin{spec}
type family Get (xs :: [(Symbol, *)]) (s :: Symbol) :: Maybe * where
    Get NIL                 s = Nothing
    Get (TPar(s, x) :- xs)  s = Just x
    Get (TPar(t, x) :- xs)  s = Get xs s {-"~~."-}
\end{spec}
%
Some other dictionary-related functions are defined in a similar fashion
in Figure \ref{fig:dict-operations}. The function |Set| either updates an
existing entry or inserts a new entry, |Del| removes an entry matching
a given key, while |Member| checks whether a given key exists in the
dictionary.

\begin{figure}
\begin{spec}
-- inserts or updates an entry
type family Set  (xs :: [(Symbol, *)]) (s :: Symbol) (x :: *) :: [(Symbol, *)] where
    Set NIL                 s x = TList (TPar (s, x))
    Set (TPar(s, y) :- xs)  s x = TPar (s, x) :- xs
    Set (TPar(t, y) :- xs)  s x = TPar (t, y) :- Set xs s x

-- removes an entry
type family Del  (xs :: [(Symbol, *)]) (s :: Symbol) :: [(Symbol, *)] where
    Del Nil                 s  = Nil
    Del (TPar (s, y) :- xs) s  = xs
    Del (TPar (t, y) :- xs) s  = TPar (t, y) :- Del xs s

-- membership
type family Member (xs :: [(Symbol, *)]) (s :: Symbol) :: Bool where
    Member Nil                  s = False
    Member (TPar(s, x) :- xs)   s = True
    Member (TPar(t, x) :- xs)   s = Member xs s
\end{spec}
\caption{Some operations on type-level dictionaries.}
\label{fig:dict-operations}
\end{figure}

\subsection{Proxies and Singleton Types}

The \Hedis{} function |del :: [ByteString] -> Either Reply Integer| takes a list
of keys (encoded to |ByteString|) and removes the entries having those keys in
the database. For simplicity, we consider creating a \Edis{} counterpart
that takes only one key. A first attempt may lead to something like the
following:
\begin{spec}
del :: String -> Edis xs (Del xs {-"~"-}?) (Either Reply Integer)
del key = Edis $ Hedis.del [encode key] {-"~~,"-}
\end{spec}
where the function |encode|, which we will see again later, converts |String|
to |ByteString| here. At term-level, our |del| merely calls |Hedis.del|. At
type-level, if the status of the database before |del| is called is specified
by the dictionary |xs|, the status afterwards should be specified by
|Del xs {-"~"-}?|. The question, however, is what to fill in place of the
question mark. It cannot be |Del xs key|, since |key| is a runtime value and
not a type. How do we pass it a runtime value to type-level?

In a language with phase distinction like Haskell, it is certainly impossible
to pass the value |key| to the type checker if it truly is a runtime value, for
example, a string read from the user. If the value of |key| can be determined
statically, however, {\em singleton types} can be used to represent a type
as a value, thus build a connection between the two realms.

A singleton type is a type that has only one term. When the term is built, it
carries a type that can be inspected by the type checker. The term can be think
of as a representative of the type at the realm of runtime values. For our
purpose, we will use the following type |Proxy|:
\begin{spec}
data Proxy t = Proxy {-"~~."-}
\end{spec}
For every type |t|, |Proxy t| is a type that has only one term: |Proxy|.
\footnote{While giving the same name to both the type and the term can be very
confusing, it is unfortunately a common practice in the Haskell community.}
To call |del|, instead of passing a key as a |String|, we give it a proxy:
\begin{spec}
del (Proxy :: Proxy "A") {-"~~,"-}
\end{spec}
where |"A"| is not a value, but a string lifted to a type (of kind |Symbol|).
Now that the type check has access to the key, the type of |del| could be
something alone the line of |del :: Proxy s -> Edis xs (Del xs s) ...|.

The next problem is that, |del|, at term level, gets only a value constructor
|Proxy| without further information, while it needs to pass a |ByteString| key
to |Hedis.del|. Every concrete string literal lifted to a type, for example
|"A"|, belongs to a type class |KnownSymbol|. For all type |n| in |KnownSymbol|,
the function |symbolVal|:
< symbolVal :: KnownSymbol n => proxy n -> String {-"~~,"-}
retrieves the string associated with a type-level literal that is known at
compile time. In summary, |del| can be implemented as:
\begin{spec}
del  :: KnownSymbol s
     => Proxy s -> Edis xs (Del xs s) (Either Reply Integer)
del key = Edis (Hedis.del [encodeKey key])  {-"~~,"-}
\end{spec}
where |encodeKey = encode . symbolVal|.
