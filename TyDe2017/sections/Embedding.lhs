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
keys in the database. For some reason to be explained later, we consider an \Edis{}
counterpart that takes only one key. A first attempt may lead to something like
the following:
\begin{spec}
del :: String -> Edis xs (Del xs {-"~"-}?) (EitherReply Integer)
del key = Edis (Hedis.del [encode key]) {-"~~,"-}
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
determined statically, however, {\em singleton types}~\cite{singletons} can be used to represent a
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
to |Hedis.del|. Every concrete string literal lifted to a type, for example,
|"A"|, belongs to a type class |KnownSymbol|. For all types |k| in |KnownSymbol|,
the function |symbolVal|:
< symbolVal :: KnownSymbol k => proxy k -> String {-"~~,"-}
retrieves the string associated with a type-level literal that is known at
compile time. In summary, |del| can be implemented as:
\begin{spec}
del ::  KnownSymbol k =>
        Proxy k -> Edis xs (Del xs k) (EitherReply Integer)
del key = Edis (Hedis.del [encodeKey key])  {-"~~,"-}
\end{spec}
where |encodeKey = encode . symbolVal|.

A final note: the function |encode|, from the Haskell library {\sc cereal},
helps to convert certain datatypes that are {\em serializable} into |ByteString|.
The function and its dual |decode| will be used more later.
\begin{spec}
encode  :: Serialize a => a -> ByteString {-"~~,"-}
decode  :: Serialize a => ByteString -> Either String a {-"~~."-}
\end{spec}

\subsection{Automatic Serialization}
\label{sec:polymorphic-redis}

As mentioned before, while \Redis{} provide a number of container types
including lists, sets, and hash, etc., the primitive type is string. \Hedis{}
programmers manually convert data of other types to strings before saving them
into the data store. In \Edis{}, we wish to save some of the effort for the
programmers, as well as keeping a careful record of the intended types of the
strings in the data store.

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
set ::  (KnownSymbol k, Serialize a) => Proxy k -> a ->
          Edis xs (Set xs k (StringOf a)) (Either Reply Status)
set key v = Edis (Hedis.set (encodeKey key) (encode v)) {-"~~,"-}
\end{spec}
For example, executing |set (Proxy :: Proxy "A") True| updates the dictionary
with an entry |TPar ("A", StringOf Bool)|. If |"A"| is not in the dictionary,
this entry is added; otherwise the old type of |"A"| is updated to
|StringOf Bool|.

\Redis{} command \texttt{INCR} reads the (string) value of the given key, parses
it as an integer, and increments it by one, before storing it back. The command
\texttt{INCRBYFLOAT} increments the floating point value of a key by a given
amount. They are defined in \Edis{} below:
\begin{spec}
incr ::  (KnownSymbol k, Get xs k ~ StringOf Integer) =>
         Proxy k -> Edis xs xs (EitherReply Integer)
incr key = Edis (Hedis.incr (encodeKey key)) {-"~~,"-}

incrbyfloat :: (KnownSymbol k, Get xs k ~ StringOf Double)
          => Proxy k -> Double -> Edis xs xs (EitherReply Double)
incrbyfloat key eps =
  Edis (Hedis.incrbyfloat (encodeKey key) eps) {-"~~."-}
\end{spec}
Notice the use of (|~|), \emph{equality constraints}~\cite{typeeq}, to enforce
that the intended type of the value of |k| must respectively be |Integer| and
|Double|. The function |incr| is only allowed to be called in a context where
the type checker is able to reduce |Get xs k| to |StringOf Integer| ---
recall that when |k| is not in |xs|, |Get xs k| cannot be fully reduced. The
type of |incrbyfloat| works in a similar way.

\subsection{Disjunctive Constraints}
\label{sec:disjunctive-constraints}

Recall, from Section \ref{sec:introduction}, that commands \texttt{LPUSH key
val} and \texttt{LLEN key} succeed either when |key| appears in the
data store and is assigned a list, or when |key| does not appear at all.
What we wish to have in their constraint is thus a predicate equivalent to |Get xs k == ListOf a |||| not (Member xs k)|. \hide{In fact, many \Redis{} commands
are invokable under such ``well-typed, or non-existent'' precondition.}

To impose a conjunctive constraint |P && Q|, one may simply put them both in the
type: |(P, Q) => ...|. Expressing disjunctive constraints is only slightly
harder, thanks to our type-level functions. We may thus write the predicate as:
\begin{spec}
Get xs k ~ ListOf a `Or` Not (Member xs k) {-"~~."-}
\end{spec}
To avoid referring to |a|, which might not exist, we define an auxiliary predicate |IsList :: * -> Bool| such that |IsList t| reduces to |TRUE|
only if |t = ListOf a|. As many \Redis{} commands are invokable only under such
``well-typed, or non-existent'' precondition, we give names to such constraints,
as seen in Figure~\ref{fig:xxxOrNX}.\\

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

type ListOrNX    xs k =
  (IsList    (Get xs k) `Or` Not (Member xs k)) ~ TRUE
type SetOrNX     xs k =
  (IsSet     (Get xs k) `Or` Not (Member xs k)) ~ TRUE
type StringOrNX  xs k =
  (IsString  (Get xs k) `Or` Not (Member xs k)) ~ TRUE
\end{spec}
\caption{The ``well-typed, or non-existent'' constraints.}
\label{fig:xxxOrNX}
\end{figure}

The \Edis{} counterpart of \texttt{LPUSH} and \texttt{LLEN} are therefore:\\
\begin{spec}
lpush ::  (KnownSymbol k, Serialize a, ListOrNX xs k) =>
          Proxy k -> a ->
            Edis xs (Set xs k (ListOf a)) (EitherReply Integer)
lpush key val =
          Edis (Hedis.lpush (encodeKey key) [encode val]) {-"~~,"-}

llen ::  (KnownSymbol k, ListOrNX xs k) =>
         Proxy k -> Edis xs xs (EitherReply Integer)
llen key = Edis (Hedis.llen (encodeKey key)) {-"~~."-}
\end{spec}
Similarly, the type of |sadd|, a function we have talked about a lot,
is given below:
\begin{spec}
sadd ::  (KnownSymbol k, Serialize a, SetOrNX xs k) =>
         Proxy k -> a ->
           Edis xs (Set xs k (SetOf a)) (EitherReply Integer)
sadd key val =
    Edis (Hedis.sadd (encodeKey key) [encode val]) {-"~~,"-}
\end{spec}

To see a command with a more complex type, consider |setnx|, which
uses the type-level function |If| defined in Section \ref{sec:type-fun}:
\begin{spec}
setnx ::  (KnownSymbol k, Serialize a) => Proxy k -> a ->
           Edis xs  (If (Member xs k) xs (Set xs k (StringOf a)))
                    (Either Reply Bool)
setnx key val =
    Edis (Hedis.setnx (encodeKey key) (encode val)) {-"~~."-}
\end{spec}
From the type one can see that |setnx key val| creates a new entry |(key,val)|
in the data store only if |key| is fresh. The type of |setnx| computes a
postcondition for static checking, as well as serving as a good documentation
for its semantics.

\subsection{Hashes}

{\em Hash} is a useful datatype supported by \Redis{}. While the \Redis{} data
store can be seen as a set of key/value pairs, a hash is itself a set of
field/value pairs. The following commands assigns a hash to key \texttt{user}.
The fields are \texttt{name}, \texttt{birthyear}, and \texttt{verified},
respectively with values \texttt{banacorn}, \texttt{1992}, and \texttt{1}.
\begin{Verbatim}[xleftmargin=.4in]
redis> hmset user name banacorn
       birthyear 1992 verified 1
OK
redis> hget user name
"banacorn"
redis> hget user birthyear
"1992"
\end{Verbatim}

For a hash to be useful, we should allow the fields to have different types. To
keep track of types of fields in a hash, |HashOf| takes a list of |(Symbol, *)|
pairs:
\begin{spec}
data HashOf :: [ (Symbol, *) ] -> * {-"~~."-}
\end{spec}
By having an entry |(k,HashOF ys)| in a dictionary, we denote that the value of
key |k| is a hash whose fields and their types are specified by |ys|, which is
also a dictionary.

\begin{figure*}[t]
\begin{spec}
type family GetHash (xs :: [ (Symbol, *) ]) (k :: Symbol) (f :: Symbol) :: * where
    GetHash (TPar (k, HashOf hs  ) :- xs)  k f = Get hs f
    GetHash (TPar (l, y          ) :- xs)  k f = GetHash xs k f

type family SetHash (xs :: [ (Symbol, *) ]) (k :: Symbol) (f :: Symbol) (a :: *) :: [ (Symbol, *) ] where
    SetHash NIL                            k f a = TPar (k, HashOf (Set NIL f a)) :- NIL
    SetHash (TPar (k, HashOf hs  ) :- xs)  k f a = TPar (k, HashOf (Set hs  f a)) :- xs
    SetHash (TPar (l, y          ) :- xs)  k f a = TPar (l, y                  ) :- SetHash xs k f a

type family DelHash (xs :: [ (Symbol, *) ]) (k :: Symbol) (f :: Symbol) :: [ (Symbol, *) ] where
    DelHash NIL                           k f = NIL
    DelHash (TPar (k, HashOf hs ) :- xs)  k f = TPar (k, HashOf (Del hs f )) :- xs
    DelHash (TPar (l, y         ) :- xs)  k f = TPar (l, y                ) :- DelHash xs k f

type family MemHash (xs :: [ (Symbol, *) ]) (k :: Symbol) (f :: Symbol) :: Bool where
    MemHash NIL                             k f = FALSE
    MemHash (TPar (k, HashOf hs   ) :- xs)  k f = Member hs f
    MemHash (TPar (k, x           ) :- xs)  k f = FALSE
    MemHash (TPar (l, y           ) :- xs)  k f = MemHash xs k f
    {-"~~"-}
\end{spec}
\vspace{-1cm}
\caption{Type-level operations for dictionaries with hashes.}
\label{fig:xxxHash}
\end{figure*}

Figure \ref{fig:xxxHash} presents some operations we need on dictionaries when
dealing with hashes. Let |xs| be a dictionary, |GetHash xs k f| returns the type
of field |f| in the hash assigned to key |k|, if both |k| and |f| exists.
|SetHash xs k f a| assigns the type |a| to the field |f| of hash |k|; if either
|f| or |k| does not exist, the hash/field is created. |Del xs k f| removes a
field, while |MemHash xs k f| checks whether the key |k| exists in |xs|, and its
value is a hash having field |f|. Their definitions make use of functions |Get|,
|Set|, and |Member| defined for dictionaries.

Once those type-level functions are defined, embedding of \Hedis{} commands for
hashes is more or less routine. For example, functions |hset| and |hget|
are shown below. Note that, instead of |hmset| (available in \Hedis{}), we
provide a function |hset| that assigns fields and values one pair at a time.
\begin{spec}
hset  :: (KnownSymbol k, KnownSymbol f,
          Serialize a, HashOrNX xs k)
      => Proxy k -> Proxy f -> a
      -> Edis xs (SetHash xs k f (StringOf a)) (EitherReply Bool)
hset key field val =
  Edis (  Hedis.hset (encodeKey key)
           (encodeKey field) (encode val)) {-"~~,"-}


hget ::  (  KnownSymbol k, KnownSymbol f, Serialize a,
            StringOf a ~ GetHash xs k f) =>
         Proxy k -> Proxy f -> Edis xs xs (EitherReply (Maybe a))
hget key field =
  Edis (  Hedis.hget (encodeKey key) (encodeKey field) >>=
          decodeAsMaybe) {-"~~,"-}
\end{spec}
where
\begin{spec}
decodeAsMaybe :: Serialize a => (EitherReply (Maybe ByteString)) ->
  Redis (EitherReply (Maybe a)) {-"~~,"-}
\end{spec}
using the function |decode|
mentioned in Section \ref{sec:proxy-key}, parses the |ByteString| in
|EitherReply (Maybe _)| to type |a|. The definition is a bit tedious but
routine.

\hide{Note that, instead of |hmset| (available in \Hedis{}), we
provide a function |hset| that assigns fields and values one pair at at time.}
We will talk about difficulties of implementing |hmset| in
Section~\ref{sec:discussions}.

\subsection{Assertions}
\label{sec:assertions}

% Consider the following scenario: We want to retrieve the value of some existing
% key, say, |"ab initio"|, with the function\footnote{The semantics of |get| in
% \Redis{} is actually more forgiving. See Section~\ref{sec:discussions}.}:
% \begin{spec}
% get  :: (KnownSymbol k, Serialize a, StringOf a ~ Get xs k)
%      => Proxy k -> Edis xs xs (EitherReply (Maybe a)) {-"~~."-}
% \end{spec}
%
% However, Haskell would complain that, the key |"ab initio"| was not
% before seen in the dictionary. So we could never write programs to retrieve the
% value of |"ab initio"|, unless its creation was witnessed by the type checker.


Finally, the creation/update behavior of \Redis{} functions is, in our opinion,
very error-prone. It might be preferable if we can explicit declare some new
keys, after ensuring that they do not already exist (in our types). This can be done below:
% It might be preferable if we can explicitly declare some new keys, under the
%  precondition that they do not already exist in our types. This can be done as
%  follows:
\begin{spec}
declare ::  (KnownSymbol k, Member xs k ~ False) =>
            Proxy k -> Proxy a -> Edis xs (Set xs k a) ()
declare key typ = Edis (return ()) {-"~~."-}
\end{spec}
% renounce :: (KnownSymbol s, Member xs s ~ True)
%         => Proxy s -> Edis xs (Del xs s) ()
% renounce s = Edis $ return () {-"~~."-}
The command |declare key typ|, where |typ| is the proxy of |a|, adds a fresh
 |key| with type |a| into the dictionary. Notice that |declare| does nothing at
 term level, but simply returns |()|, since it only has effects on types.

% The key is not actually created yet.
% The declaration, however, ensures that if |key| is actually created, perhaps
% by one of those ``well-typed or non-existent'' command, its type must be |a|.
% The command |start| initializes the dictionary to the empty list:

Another command for type level assertion, |start|, initializes the dictionary to
 the empty list, comes in handy when starting a series of \Edis{} commands:

\begin{spec}
start :: Edis NIL NIL ()
start = Edis (return ()) {-"~~."-}
\end{spec}

\subsection{A Slightly Larger Example}

We present a slightly larger example as a summary. The task is to store a queue of
messages in \Redis{}. Messages are represented by a |ByteString| and an
|Integer| identifier:%
\footnote{|Message| is made an instance of |Generic| in order to use the
generic implementation of methods of |Serialize|.}
\begin{spec}
data Message = Msg ByteString Integer
                   deriving (Show, Generic) {-"~~,"-}
instance Serialize Message where  {-"~~."-}
\end{spec}

In the data store, the queue is represented by a list. Before pushing a message
into the queue, we increment |counter|, a key storing a counter, and use it as the
identifier of the message:
\begin{spec}
push ::  (   StringOfIntegerOrNX xs "counter",
             ListOrNX xs "queue") =>
         ByteString -> Edis xs (Set xs "queue"
          (ListOf Message)) (EitherReply Integer)
push msg =  incr kCounter `bind` \i ->
            lpush kQueue (Msg msg (fromRight i)) {-"~~,"-}
\end{spec}%​
where |fromRight :: Either a b -> b| extracts the value wrapped by constructor
|Right|, and the constraint |StringOfIntegerOrNX| |xs k| holds if either |k|
appears in |xs| and is converted from an |Integer|, or |k| does not
appear in |xs|. For brevity, the proxies are given names: \\
% \noindent{\centering %\small
% \begin{minipage}[b]{0.4\linewidth}
\begin{spec}
kCounter ::  Proxy "counter"
kCounter =   Proxy {-"~~,"-}
\end{spec}
% \end{minipage}
% \begin{minipage}[b]{0.4\linewidth}
\begin{spec}
kQueue ::  Proxy "queue"
kQueue =   Proxy {-"~~."-}
\end{spec}
% \end{minipage}}\\
To pop a message we use the function |rpop| which, given a key associated with
a list, extracts the rightmost element of the list
\begin{spec}
pop ::  (Get xs "queue" ~ ListOf Message) =>
        Edis xs xs (EitherReply (Maybe Message))
pop = rpop kQueue {-"~~,"-}

rpop ::  (KnownSymbol k, Serialize a, Get xs k ~ ListOf a) =>
         Proxy k -> Edis xs xs (EitherReply (Maybe a))
rpop key = Edis (  Hedis.rpop (encodeKey key) >>=
                   decodeAsMaybe) {-"~~."-}
\end{spec}
%
Our sample program is shown below:
\begin{spec}
prog =  declare kCounter  (Proxy :: Proxy Integer)
   >>>  declare kQueue    (Proxy :: Proxy (ListOf Message))
   >>>  push "hello"
   >>>  push "world"
   >>>  pop {-"~~,"-}
\end{spec}
where the monadic sequencing operator |(>>>)| is defined by:
\begin{spec}
(>>>) :: IMonad m => m p q a -> m q r b -> m p r b
m1 >>> m2 = m1 `bind` (\ _ -> m2) {-"~~."-}
\end{spec}
Use of |declare| in |prog| ensures that neither |"counter"| nor |"queue"| exist
before the execution of |prog|. The program simply stores two strings in |"queue"|, before extracting the first string. GHC is able to infer the type of |prog|:
\begin{spec}
prog :: Edis  NIL (  TList (TPar ("counter", Integer),
                     {-"\quad\!"-} TPar("queue", ListOf Message)))
              (EitherReply (Maybe Message)) {-"~~."-}
\end{spec}

To get things going, the main program builds a connection with the \Redis{}
server, runs |prog|, and prints the result:
\begin{spec}
main :: IO ()
main = do  conn    <- connect defaultConnectInfo
           result  <- runRedis conn (unEdis (start >>> prog))
           print result {-"~~."-}
\end{spec}
The command |start| in |main| guarantees that the
program is given a fresh run without previously defined keys at all.
All type-level constraints in |start >>> prog| are stripped away
by |unEdis|. The untyped program stored in |Edis|, of type |Redis (EitherReply (Maybe Message))|, is passed to the \Redis{} function
|runRedis|, of type |Connection -> Redis a -> IO a|. In this case the output
is |Right (Just "hello")|.

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
