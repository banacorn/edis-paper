%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Embedding \Hedis{} Commands}
\label{sec:embedding-commands}

Having the indexed monads and type-level dictionaries, in this section we
present our embedding of \Hedis{} commands into \Edis{}, while introducing
necessary concepts when they are used.

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
where the function |encode| converts |String| to |ByteString|. At term-level,
our |del| merely calls |Hedis.del|. At
type-level, if the status of the database before |del| is called is specified
by the dictionary |xs|, the status afterwards should be specified by
|Del xs {-"~"-}?|. The question, however, is what to fill in place of the
question mark. It cannot be |Del xs key|, since |key| is a runtime value and
not a type. How do we smuggle a runtime value to type-level?

In a language with phase distinction like Haskell, it is certainly impossible
to pass the value of |key| to the type checker if it truly is a runtime value,
for example, a string read from the user. If the value of |key| can be
determined statically, however, {\em singleton types} can be used to represent a
type as a value, thus build a connection between the two realms.

A singleton type is a type that has only one term. When the term is built, it
carries a type that can be inspected by the type checker. The term can be think
of as a representative of its type at the realm of runtime values. For our
purpose, we will use the following type |Proxy|:
\begin{spec}
data Proxy t = Proxy {-"~~."-}
\end{spec}
For every type |t|, |Proxy t| is a type that has only one term: |Proxy|.
\footnote{While giving the same name to both the type and the term can be very
confusing, it is unfortunately a common practice in the Haskell community.}
To call |del|, instead of passing a key as a |String|, we give it a proxy with
a specified type:
\begin{spec}
del (Proxy :: Proxy "A") {-"~~,"-}
\end{spec}
where |"A"| is not a value, but a string lifted to a type (of kind |Symbol|).
Now that the type checker has access to the key, the type of |del| could be
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

A final note: functions the |encode|, from the Haskell library {\sc cereal},
helps to convert certain datatype that are {\em serializable} into |ByteString|.
The function and its dual |decode| will be use more later.
\begin{spec}
encode  :: Serialize a => a -> ByteString {-"~~,"-}
decode  :: Serialize a => ByteString -> Either String a {-"~~."-}
\end{spec}

\subsection{Storing Primitive Datatypes Other Than Strings}
\label{sec:polymorphic-redis}

As mentioned before, while \Redis{} provide a number of container types
including lists, sets, and hash, etc., the primitive type is string.
\Hedis{} programmers manually convert other types of data to strings before
saving them into the data store. In \Edis{}, we wish to save some of the
effort for the programmers, as well as keeping a careful record of the intended
types of the strings in the data store.

To keep track of intended types of strings in the data store, we define the
following types (that have no terms):
\begin{spec}
data StringOf x {-"~~,"-}
data ListOf x {-"~~,"-}
data SetOf x {-"~~..."-}
\end{spec}
If an key is associated with, for example, |StringOf Int| in
our dictionary, we mean that its associated value in the data store was
serialized from an |Int| and should be used as an |Int|. Types
|ListOf x| and |SetOf x|, respectively, denotes that the value is a list
or a set of the given type.

While the |set| command in \Hedis{} always writes a string to the data store,
the corresponding |set| in \Redis{} applies to any serializable type (those
in the class |Serialize|), and performs the encoding for the user:
\begin{spec}
set  :: (KnownSymbol s, Serialize x)
     => Proxy s -> x -> Edis xs (Set xs s (StringOf x)) (Either Reply Status)
set key val = Edis $ Hedis.set (encodeKey key) (encode val) {-"~~,"-}
\end{spec}
where the function |encode :: Serialize a => a -> ByteString|

For example, executing |set (Proxy :: Proxy "A") True| updates the dictionary
with an entry |TPar ("A", StringOf Bool)|: if |"A"| is not in the dictionary,
the entry is added; otherwise the old type of |"A"| is updated to
|StringOf Bool|.

\Redis{} command \texttt{INCR} reads the string associated with the given key,
parse it as an integer, and increments it, before storing it back. The command
\texttt{INCRBYFLOAT} increments the floating number, associated to the given
key. They can be
\begin{spec}
incr  :: (KnownSymbol s, Get xs s ~ Just (StringOf Integer))
      => Proxy s -> Edis xs xs (Either Reply Integer)
incr key = Edis $ Hedis.incr (encodeKey key) {-"~~,"-}

incrbyfloat  :: (KnownSymbol s, Get xs s ~ Just (StringOf Double))
             => Proxy s -> Double -> Edis xs xs (Either Reply Double)
incrbyfloat key eps = Edis $ Hedis.incrbyfloat (encodeKey key) eps {-"~~."-}
\end{spec}
Notice the use of (|~|), \emph{equality constraints}~\cite{typeeq}, to enforce
that the intended type of value associated with key |s| must respectively be
|Integer| and |Double|.

\todo{Some more interesting functions that uses IF and FromJust, etc.
What about |lset|?}

\subsection{Constraint Disjunctions}

In the following example, \texttt{LPUSH} appends a string \texttt{"bar"} to the
list associated to \texttt{some-list}, or creates a list if \texttt{some-list}
does not yet present. The command \texttt{LLEN} then returns the length of
the list. If the key passed to \texttt{LLEN} is not associated to a list,
it signals an error.
\begin{verbatim}
redis> LPUSH some-list bar
(integer) 1
redis> LLEN some-list
(integer) 1
redis> SET some-string foo
OK
redis> LLEN some-string
(error) WRONGTYPE Operation against a key holding
the wrong kind of value
\end{verbatim}
One may give |llen| this type:
\begin{spec}
llen  :: (KnownSymbol s, Get xs s ~ Just (ListOf x))
      => Proxy s -> Edis xs xs (Either Reply Integer)
llen key = Edis $ Hedis.llen (encodeKey key) {-"~~."-}
\end{spec}
However, this is not an accurate specification of \texttt{LLEN} in \Redis{} ---
\texttt{LLEN} also accepts keys that do not exist, and replies with \texttt{0}:
\begin{verbatim}
redis> LLEN nonexistent
(integer) 0
\end{verbatim}
What we wish to have in the class constraint of |llen| is thus a predicate
equivalent to |Get xs s == Just (ListOf x) |||| not (Member xs
s)|. The situation is the same with |lpush|: it returns normally either when
the key is associated to a list or does not exist at all, while signaling an
error when the associated value is not a list.

Unfortunately, expressing disjunctions in constraints is much more difficult
than expressing conjunctions. To impose a class constraint |P && Q|, one may
simply put them both in the type: |(P, Q) => ...|. To express top-level
constraints

\todo{why cite \cite{singletons} here?}

We could achieve this simply by translating the semantics we want to the
 domain of Boolean, with type-level boolean functions such as
|(&&)|,
|(||)|, |Not|,
|(==)|, etc.\footnotemark To avoid

\footnotetext{Available in \text{Data.Type.Bool} and
 \text{Data.Type.Equality}}

\begin{spec}
Get xs s == Just (ListOf x) || Not (Member xs s)
\end{spec}

To avoid addressing the type of value (as it may not exist at all), we defined
 an auxiliary predicate |IsList :: Maybe * -> Bool| to
 replace the former part.

\begin{spec}
IsList (Get xs s) || Not (Member xs s)
\end{spec}

The type expression above has kind |Bool|, we could make it
 a type constraint by asserting equality.

\begin{spec}
(IsList (Get xs s) || Not (Member xs s)) ~ True
\end{spec}

With \emph{constraint kind}, a recent addition to GHC, type constraints now has
 its own kind: |Constraint|. That means type constraints
 are not restricted to the left side of a |=>| anymore,
 they could appear in anywhere that accepts something of kind
 |Constraint|, and any type that has kind
 |Constraint| can also be used as a type constraint.
 \footnote{See \url{https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/constraint-kind.html}.}

As many other list-related commands also have this ``List or nothing'' semantics,
 we could abstract the lengthy type constraint above and give it an alias with
 type synonym.

\begin{spec}
ListOrNX xs s =
    (IsList (Get xs s) || Not (Member xs s)) ~ True
\end{spec}

The complete implementation of \text{LLEN} with
|ListOrNX| would become:

\begin{spec}
llen :: (KnownSymbol s, ListOrNX xs s)
        => Proxy s
        -> Edis xs xs (Either Reply Integer)
llen key = Edis $ Hedis.llen (encodeKey key)
\end{spec}
