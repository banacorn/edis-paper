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
For every type |t|, |Proxy t| is a type that has only one term: |Proxy|.%
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

A final note: the function |encode|, from the Haskell library {\sc cereal},
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
For example, executing |set (Proxy :: Proxy "A") True| updates the dictionary
with an entry |TPar ("A", StringOf Bool)|. If |"A"| is not in the dictionary,
this entry is added; otherwise the old type of |"A"| is updated to
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
|Integer| and |Double|. The function |incr| is allowed to be called only
in a context where the type checker is able to reduce |Get xs s| to
|Just (StringOf Integer)|. Similarly with |incrbyfloat|.

\todo{Some more interesting functions that uses IF and FromJust, etc.
What about |lset|?}

\subsection{Constraint Disjunctions}

Recall, from Section \ref{sec:introduction}, that commands \texttt{LPUSH key
val} and \texttt{LLEN key} returns normally either when |key| does not present
in the data store, or when |key| presents and is associated to a list.
What we wish to have in their constraint is thus a predicate equivalent to |Get xs s == Just (ListOf x) |||| not (Member xs s)|.

To impose a conjunctive constraint |P && Q|, one may simply put them both in the
type: |(P, Q) => ...|. Expressing disjunctive constraints is only slightly
harder, thanks to our type-level functions. Various operators for type-level
boolean and equality are defined in \text{Data.Type.Bool} and
\text{Data.Type.Equality}, like how we defined |Or| in Section
\ref{sec:type-fun}. We may thus write the predicate as:
\begin{spec}
Get xs s == Just (ListOf x) `Or` Not (Member xs s) {-"~~."-}
\end{spec}
To avoid referring to |x|, which might not exist, we define an auxiliary predicate |IsList :: Maybe * -> Bool|:
\begin{spec}
type family IsList (x :: *) :: Bool where
    IsList (ListOf n) = TRUE
    IsList x          = FALSE {-"~~."-}
\end{spec}
As many other list-related commands also have this ``List or nothing'' semantics, we give the type constraint a name:
\begin{spec}
ListOrNX xs s = (IsList (Get xs s) || Not (Member xs s)) ~ True {-"~~."-}
\end{spec}
\noindent The complete implementation of \text{LLEN} with
|ListOrNX| is therefore:
\begin{spec}
lpush :: (KnownSymbol s, Serialize x, ListOrNX xs s)
      => Proxy s -> x -> Edis xs (Set xs s (ListOf x)) (Either Reply Integer)
lpush key val = Edis $ Redis.lpush (encodeKey key) [encode val] {-"~~,"-}

llen  :: (KnownSymbol s, ListOrNX xs s)
      => Proxy s -> Edis xs xs (Either Reply Integer)
llen key = Edis $ Hedis.llen (encodeKey key) {-"~~."-}
\end{spec}



\todo{why cite \cite{singletons} here?}

% The type expression above has kind |Bool|, we could make it
%  a type constraint by asserting equality.
%
% With \emph{constraint kind}, a recent addition to GHC, type constraints now has
%  its own kind: |Constraint|. That means type constraints
%  are not restricted to the left side of a |=>| anymore,
%  they could appear in anywhere that accepts something of kind
%  |Constraint|, and any type that has kind
%  |Constraint| can also be used as a type constraint.
%  \footnote{See \url{https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/constraint-kind.html}.}

\subsection{Assertions}
\label{sec:assertions}

Finally, the creation/update behavior of \Redis{} functions is, in our opinion,
very error-prone. It might be preferable if we can explicit declare some new
keys, after ensure that they do not already exist (in our types), and renounce
them and forbid further access when we are sure that they shall not be referred
to anymore. This can be done below:
\begin{spec}
declare :: (KnownSymbol s, Member xs s ~ False)
        => Proxy s -> Proxy x -> Edis xs (Set xs s x) ()
declare s x = Edis $ return () {-"~~,"-}

renounce :: (KnownSymbol s, Member xs s ~ True)
        => Proxy s -> Edis xs (Del xs s) ()
renounce s = Edis $ return () {-"~~."-}
\end{spec}
The command |declare s x| adds a new key |s| with type |x| to the dictionary,
if it does not already exist. Dually, |renounce| removes a key from the
dictionary. Even though it may still exist in the data store, it is not allowed
to be referred to. The command |start| initializes the dictionary to |NIL|:
\begin{spec}
start :: Edis NIL NIL ()
start = Edis $ return () {-"~~."-}
\end{spec}

\subsection{A Larger Example}

The following program increases the value of |"A"| as an integer, push the result of the increment to list |"L"|, and then pops it out:
\begin{spec}
main :: IO ()
main = do
    conn    <- connect defaultConnectInfo
    result  <- runRedis conn $ unEdis $ start
        `bind` \ _ ->  declare (Proxy :: Proxy "A") (Proxy :: Proxy Integer)
        `bind` \ _ ->  incr (Proxy :: Proxy "A")
        `bind` \n ->  case n of
            Left  err  -> lpush (Proxy :: Proxy "L") 0
            Right n    -> lpush (Proxy :: Proxy "L") n
        `bind` \ _ ->  lpop     (Proxy :: Proxy "L")
    print result
\end{spec}

The syntax is pretty heavy, like the old days when there's no
 \emph{do-notation}\cite{history}. But if we don't need any variable bindings
 between operations, we could compose these commands with a sequencing operator
 |(>>>)|.

\begin{spec}
(>>>) :: IMonad m => m p q a -> m q r b -> m p r b
\end{spec}
\begin{spec}
program = start
    >>> declare (Proxy :: Proxy "A") (Proxy :: Proxy Integer)
    >>> incr    (Proxy :: Proxy "A")
    >>> lpush   (Proxy :: Proxy "L") 0
    >>> lpop    (Proxy :: Proxy "L")
\end{spec}