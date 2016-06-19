%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Embedding \Hedis{} Commands}
\label{sec:embedding-commands}

Having the indexed monads and type-level dictionaries, in this section we
present our embedding of \Hedis{} commands into \Edis{}, while introducing
necessary concepts when they are used.

\subsection{Proxies and Singleton Types}
\label{sec:proxy-key}

The \Hedis{} function |del :: [ByteString] -> Redis (EitherReply Integer)| takes a list of keys (encoded to |ByteString|) and removes the entries having those
keys in the database. For reason to be explained later, we consider an \Edis{}
counterpart that takes only one key. A first attempt may lead to something like
the following:
\begin{spec}
del :: String -> Edis xs (Del xs {-"~"-}?) (EitherReply Integer)
del key = Edis $ Hedis.del [encode key] {-"~~,"-}
\end{spec}
where the function |encode| converts |String| to |ByteString|. At term-level,
our |del| merely calls |Hedis.del|. At type-level, if the status of the database
before |del| is called meets the constraint represented by the dictionary
|xs|, the status afterwards should meet the constraint |Del xs {-"~"-}?|. The question, however, is what to fill in place of the question mark. It cannot be
|Del xs key|, since |key| is a runtime value and not a type. How do we smuggle
a runtime value to type-level?

In a language with phase distinction like Haskell, it is certainly impossible
to pass the value of |key| to the type checker if it truly is a runtime value,
for example, a string read from the user. If the value of |key| can be
determined statically, however, {\em singleton types} can be used to represent a
type as a value, thus build a connection between the two realms.

A singleton type is a type that has only one term. When the term is built, it
carries a type that can be inspected by the type checker. The term can be
thought of as a representative of its type at the realm of runtime values. For
our purpose, we will use the following type |Proxy|:
\begin{spec}
data Proxy t = Proxy {-"~~."-}
\end{spec}
For every type |t|, |Proxy t| is a type that has only one term: |Proxy|.%
\footnote{While giving the same name to both the type and the term can be very
confusing, it is unfortunately a common practice in the Haskell community.}
To call |del|, instead of passing a key as a |String| value, we give it a proxy
with a specified type:
\begin{spec}
del (Proxy :: Proxy "A") {-"~~,"-}
\end{spec}
where |"A"| is not a value, but a string lifted to a type (of kind |Symbol|).
Now that the type checker has access to the key, the type of |del| can be
|Proxy k -> Edis xs (Del xs k) (EitherReply Integer)|.

The next problem is that, |del|, at term level, gets only a value constructor
|Proxy| without further information, while it needs to pass a |ByteString| key
to |Hedis.del|. Every concrete string literal lifted to a type, for example
|"A"|, belongs to a type class |KnownSymbol|. For all type |k| in |KnownSymbol|,
the function |symbolVal|:
< symbolVal :: KnownSymbol k => proxy k -> String {-"~~,"-}
retrieves the string associated with a type-level literal that is known at
compile time. In summary, |del| can be implemented as:
\begin{spec}
del  :: KnownSymbol k
     => Proxy k -> Edis xs (Del xs k) (EitherReply Integer)
del key = Edis (Hedis.del [encodeKey key])  {-"~~,"-}
\end{spec}
where |encodeKey = encode . symbolVal|.

A final note: the function |encode|, from the Haskell library {\sc cereal},
helps to convert certain datatype that are {\em serializable} into |ByteString|.
The function and its dual |decode| will be used more later.
\begin{spec}
encode  :: Serialize a => a -> ByteString {-"~~,"-}
decode  :: Serialize a => ByteString -> Either String a {-"~~."-}
\end{spec}

\subsection{Automatic Serialization}
\label{sec:polymorphic-redis}

As mentioned before, while \Redis{} provide a number of container types
including lists, sets, and hash, etc., the primitive type is string.
\Hedis{} programmers manually convert data of other types to strings before
saving them into the data store. In \Edis{}, we wish to save some of the
effort for the programmers, as well as keeping a careful record of the intended
types of the strings in the data store.

To keep track of intended types of strings in the data store, we define the
following types (that have no terms):
\begin{spec}
data StringOf  :: * -> * {-"~~,"-}
data ListOf    :: * -> * {-"~~,"-}
data SetOf     :: * -> * {-"~~..."-}
\end{spec}
If a key is associated with, for example, |StringOf Int| in our dictionary, we
mean that its value in the data store was serialized from an |Int| and should be
used as an |Int|. Types |ListOf a| and |SetOf a|, respectively, denotes that the
value is a list or a set of type |a|.

While the |set| command in \Hedis{} always writes a string to the data store,
the corresponding |set| in \Redis{} applies to any serializable type (those
in the class |Serialize|), and performs the encoding for the user:
\begin{spec}
set  :: (KnownSymbol k, Serialize a)
     => Proxy k -> a -> Edis xs (Set xs k (StringOf a)) (Either Reply Status)
set key val = Edis $ Hedis.set (encodeKey key) (encode val) {-"~~,"-}
\end{spec}
For example, executing |set (Proxy :: Proxy "A") True| updates the dictionary
with an entry |TPar ("A", StringOf Bool)|. If |"A"| is not in the dictionary,
this entry is added; otherwise the old type of |"A"| is updated to
|StringOf Bool|.

\Redis{} command \texttt{INCR} reads the (string) value of the given key, parse
it as an integer, and increments it by one, before storing it back. The command
\texttt{INCRBYFLOAT} increments the floating point value of a key by a given
amount. They are defined in \Edis{} below:
\begin{spec}
incr  :: (KnownSymbol k, Get xs k ~ Just (StringOf Integer))
      => Proxy k -> Edis xs xs (EitherReply Integer)
incr key = Edis $ Hedis.incr (encodeKey key) {-"~~,"-}

incrbyfloat  :: (KnownSymbol k, Get xs k ~ Just (StringOf Double))
             => Proxy k -> Double -> Edis xs xs (EitherReply Double)
incrbyfloat key eps = Edis $ Hedis.incrbyfloat (encodeKey key) eps {-"~~."-}
\end{spec}
Notice the use of (|~|), \emph{equality constraints}~\cite{typeeq}, to enforce
that the intended type of value of |k| must respectively be |Integer| and
|Double|. The function |incr| is allowed to be called only in a context where
the type checker is able to reduce |Get xs k| to |Just (StringOf Integer)|. Similarly with |incrbyfloat|.

\subsection{Disjunctive Constraints}
\label{sec:disjunctive-constraints}

Recall, from Section \ref{sec:introduction}, that commands \texttt{LPUSH key
val} and \texttt{LLEN key} returns normally either when |key| presents in the
data store and is associated to a list, or when |key| does not present at all.
What we wish to have in their constraint is thus a predicate equivalent to |Get xs k == Just (ListOf a) |||| not (Member xs k)|. In fact, many \Redis{} commands
are invokable under such ``well-typed, or non-existent'' precondition.

To impose a conjunctive constraint |P && Q|, one may simply put them both in the
type: |(P, Q) => ...|. Expressing disjunctive constraints is only slightly
harder, thanks to our type-level functions. We may thus write the predicate as:
\begin{spec}
Get xs k ~ Just (ListOf a) `Or` Not (Member xs k) {-"~~."-}
\end{spec}
To avoid referring to |a|, which might not exist, we define an auxiliary predicate |IsList :: * -> Bool| such that |IsList t| reduces to |TRUE|
only if |t = ListOf a|. As many \Redis{} commands are invokable only under such
``well-typed, or non-existent'' precondition, we give names to such constraints,
as seen in Figure~\ref{fig:xxxOrNX}.

\begin{figure}[t]
\begin{spec}
type family IsList (t :: *) :: Bool where
    IsList (ListOf a)  = TRUE
    IsList t           = FALSE
type family IsSet (t :: *) :: Bool where
    IsSet (SetOf a)  = TRUE
    IsSet t          = FALSE
type family IsString (t :: *) :: Bool where
    IsString (StringOf a)  = TRUE
    IsString t             = FALSE

type family FromJust (x :: Maybe k) :: k where
    FromJust (JUST k) = k

type ListOrNX    xs k =
  (IsList    (FromJust (Get xs k)) `Or` Not (Member xs k)) ~ TRUE
type SetOrNX     xs k =
  (IsSet     (FromJust (Get xs k)) `Or` Not (Member xs k)) ~ TRUE
type StringOrNX  xs k =
  (IsString  (FromJust (Get xs k)) `Or` Not (Member xs k)) ~ TRUE
\end{spec}
\caption{The ``well-typed, or non-existent'' constraints.}
\label{fig:xxxOrNX}
\end{figure}

The \Edis{} counterpart of \texttt{LPUSH} and \texttt{LLEN} are therefore:
\begin{spec}
lpush  :: (KnownSymbol k, Serialize a, ListOrNX xs k)
       => Proxy k -> a -> Edis xs (Set xs k (ListOf a)) (EitherReply Integer)
lpush key val = Edis $ Redis.lpush (encodeKey key) [encode val] {-"~~,"-}

llen  :: (KnownSymbol k, ListOrNX xs k)
      => Proxy k -> Edis xs xs (EitherReply Integer)
llen key = Edis $ Hedis.llen (encodeKey key) {-"~~."-}
\end{spec}
Similarly, the type of |sadd|, a function we have talked about a lot,
is given below:
\begin{spec}
sadd  :: (KnownSymbol k, Serialize a, SetOrNX xs k)
      => Proxy k -> a -> Edis xs (Set xs k (SetOf a)) (EitherReply Integer)
sadd key val = Edis $ Redis.sadd (encodeKey key) [encode val] {-"~~,"-}
\end{spec}

To see a command with a more complex type, consider |setnx|, which
uses the type-level function |If| defined in Section \ref{sec:type-fun}:
\begin{spec}
setnx  :: (KnownSymbol k, Serialize a)
       => Proxy k -> a -> Edis xs  (If (Member xs k) xs (Set xs k (StringOf a)))
                                   (Either Reply Bool)
setnx key val = Edis $ Redis.setnx (encodeKey key) (encode val) {-"~~."-}
\end{spec}
From the type one can see that |setnx key val| creates a new entry |(key,val)|
in the data store only if |key| is fresh. The type of |setnx| computes a
postcondition for static checking, as well as serving as a good documentation
for its semantics.

\subsection{Assertions}
\label{sec:assertions}

Finally, the creation/update behavior of \Redis{} functions is, in our opinion,
very error-prone. It might be preferable if we can explicit declare some new
keys, after ensure that they do not already exist (in our types). This can be done below:
\begin{spec}
declare  :: (KnownSymbol k, Member xs a ~ False)
         => Proxy k -> Proxy a -> Edis xs (Set xs k a) ()
declare key val = Edis $ return () {-"~~."-}
\end{spec}
% renounce :: (KnownSymbol s, Member xs s ~ True)
%         => Proxy s -> Edis xs (Del xs s) ()
% renounce s = Edis $ return () {-"~~."-}
The command |declare key val| adds a fresh |key| with type |a| and value
|val|. The type ensure that |key| does not already exist. The command |start| initializes the dictionary to the empty list:
\begin{spec}
start :: Edis NIL NIL ()
start = Edis $ return () {-"~~."-}
\end{spec}

\subsection{A Larger Example}

As a summary, we present a larger example. The following main program build
a connection with the \Redis{} server, runs an embedded program |prog|, and
prints the result:
\begin{spec}
main :: IO ()
main = do
    conn    <- connect defaultConnectInfo
    result  <- runRedis conn $ unEdis $ prog
    print result {-"~~."-}
\end{spec}
The embedded program |prog| increases the value of |"A"| as an integer, push the incremented value to list |"L"|, and then pops it out:
\begin{spec}
prog :: Edis NIL (TList (TPar ("A", StringOf Integer), TPar ("L", ListOf Integer))) Integer
prog =  start
        >>>     declare kA tInteger
        >>>     incr kA
        `bind`  \ n -> case n of
                   Left err  -> lpush kL 0
                   Right n   -> lpush kL n
        >>>     lpop kL
  where  (kA, kL, tInteger) =
          (Proxy :: Proxy "A", Proxy :: Proxy "L", Proxy :: Proxy Integer) {-"~~."-}
\end{spec}
The sequencing operator |(>>>)| is defined by:
\begin{spec}
(>>>) :: IMonad m => m p q a -> m q r b -> m p r b
m1 >>> m2 = m1 `bind` (const m2) {-"~~."-}
\end{spec}

% The following program increases the value of |"A"| as an integer, push the result of the increment to list |"L"|, and then pops it out:
% \begin{spec}
% main :: IO ()
% main = do
%     conn    <- connect defaultConnectInfo
%     result  <- runRedis conn $ unEdis $ start
%         `bind` \ _ ->  declare (Proxy :: Proxy "A") (Proxy :: Proxy Integer)
%         `bind` \ _ ->  incr (Proxy :: Proxy "A")
%         `bind` \n ->  case n of
%             Left  err  -> lpush (Proxy :: Proxy "L") 0
%             Right n    -> lpush (Proxy :: Proxy "L") n
%         `bind` \ _ ->  lpop     (Proxy :: Proxy "L")
%     print result
% \end{spec}

% The syntax is pretty heavy, like the old days when there's no
%  \emph{do-notation}\cite{history}. But if we don't need any variable bindings
%  between operations, we could compose these commands with a sequencing operator
%  |(>>>)|.


% \begin{spec}
% program = start
%     >>> declare (Proxy :: Proxy "A") (Proxy :: Proxy Integer)
%     >>> incr    (Proxy :: Proxy "A")
%     >>> lpush   (Proxy :: Proxy "L") 0
%     >>> lpop    (Proxy :: Proxy "L")
% \end{spec}

\todo{Give a more interesting example?}
