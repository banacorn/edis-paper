%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Introduction}
\label{sec:introduction}

\Redis{}\footnote{\url{https://redis.io}} is an open source, in-memory data
structure store that can be used as database, cache, and message broker.
A \Redis{} data store can be thought of as a set of key-value pairs. The value
can be a string, a list of strings, a set of strings, or a hash table of
strings, etc. However, string is the only primitive datatype. Numbers, for
example, are serialized to strings before being saved in the data store, and
parsed back to numbers to be manipulated with. While the concept is simple,
\Redis{} is used as an essential component in a number of popular, matured services, including Twitter, GitHub, Weibo, StackOverflow, and Flickr.

For an example, consider the following sequence of commands, entered through the
interactive interface of \Redis{}. The keys \texttt{some-set} and
\texttt{another-set} are both assigned a set of strings. The two calls to
command \texttt{SADD} respectively add three and two strings to the two sets,
before \texttt{SINTER} takes their intersection:
\begin{Verbatim}[xleftmargin=.4in]
redis> SADD some-set a b c
(integer) 3
redis> SADD another-set a b
(integer) 2
redis> SINTER some-set another-set
1) "a"
2) "b"
\end{Verbatim}
\noindent Notice that the keys \texttt{some-set} and \texttt{another-set}, if
not existing before the call to \texttt{SADD}, are created on site. The calls to
\texttt{SADD} return the sizes of the resulting sets.

Many third party libraries provide interfaces for general purpose programming
languages to access \Redis{} through its TCP protocol. For Haskell, the most
popular library is
\Hedis{}.\footnote{\url{https://hackage.haskell.org/package/hedis}}
A (normal) \Redis{} computation returning a value of type |a| is represented in
Haskell by |Redis (Either Reply a)|, where the type |Redis| is a monad, while
|Either Reply a| indicates that the computation either returns a value of type
|a|, or fails with an error message represented by type |Reply|. The following
program implements the previous example:
\begin{spec}
program :: Redis (Either Reply [ByteString])
program = do
    sadd "some-set" ["a", "b"]
    sadd "another-set" ["a", "b", "c"]
    sinter ["some-set", "another-set"] {-"~~."-}
\end{spec}
The function
\begin{spec}
sadd :: ByteString -> [ByteString] ->
        Redis (Either Reply Integer)
\end{spec}
takes a key and a list of values as arguments, and returns a
\Redis{} computation yielding |Integer|. Keys and values, being nothing but
binary strings in \Redis{}, are represented using Haskell |ByteString|.

\paragraph{The Problems} Most commands work only with data of certain types. In
the following example, the key \texttt{some-string} is assigned a string
\texttt{foo} --- the command \texttt{SET} always assigns a string to a key.
The subsequent call to \texttt{SADD}, which adds a value to a set, thus raises a
runtime error.
\begin{Verbatim}[xleftmargin=.4in]
redis> SET some-string foo
OK
redis> SADD some-string bar
(error) WRONGTYPE Operation against a key
holding the wrong kind of value
\end{Verbatim}
\noindent For another source of type error, the command \texttt{INCR key}
increments the value associated with \texttt{key} by one. With strings being the
only primitive type, \Redis{} parses the stored string to an integer and, after
incrementation, stores a string back. If the string cannot be parsed as an
integer, a runtime error is raised.

We point out a peculiar pattern of value creation and updating
in \Redis{}: the same command is used both to create a key-value pair and to
update them. Similar to \texttt{SADD}, the command \texttt{LPUSH} appends a
value (a string) to a list, or creates one if the key does not exist:
\begin{Verbatim}[xleftmargin=.4in]
redis> LPUSH some-list bar
(integer) 1
\end{Verbatim}
\noindent Another command \texttt{LLEN} returns the length of the list, and
signals an error if the key is not associated with a list:
\begin{Verbatim}[xleftmargin=.4in]
redis> LLEN some-list
(integer) 1
redis> SET some-string foo
OK
redis> LLEN some-string
(error) WRONGTYPE Operation against a key
holding the wrong kind of value
\end{Verbatim}
\noindent Curiously, however, when applied to a key not yet created, \Redis{}
designers chose to let \texttt{LLEN} return \texttt{0}:
\begin{Verbatim}[xleftmargin=.4in]
redis> LLEN nonexistent
(integer) 0
\end{Verbatim}

Being a simple wrapper on top of the TCP protocol of \Redis{}, \Hedis{}
inherits all the behaviors. Executing the following program yields the same
error, but wrapped in a Haskell constructor: |Left (Error| \texttt{"WRONGTYPE
Operation against a key holding the wrong kind of value"}|)|.
\begin{spec}
program :: Redis (Either Reply Integer)
program = do
    set "some-string" "foo"
    sadd "some-string" ["a"] {-"~~."-}
\end{spec}

Such a programming model is certainly very error-prone. Working within Haskell,
a host language with a strong typing system, one naturally wishes to build a
domain-specific embedded language (DSEL) that exploits the rich type system
of Haskell to not only ensure the absence of \Redis{} type errors (at least in the simplified situation where there is one client accessing the store), but also
provides better documentations. We wish to be sure that a program calling
\texttt{INCR}, for example, can be type checked only if we can statically
guarantee that the value to be accessed is indeed an integer. We wish to see
from the type of operators such as \texttt{LLEN} when it can be called, and
allow it to be used only in contexts that are safe. We may even want to
explicitly declare a fresh key and its type, to avoid reusing an existing key
by accident, and to prevent it from unexpectedly being used as some other type.

This paper discusses the techniques we used and the experiences we learned from
building such a language, \Edis{}. We constructed an {\em indexed monad}, on
top of the monad |Redis|, which is indexed by a dictionary that maintains the
set of currently defined keys and their types. To represent the dictionary, we
need to encode variable bindings with {\em type-level} lists and strings. And
to manipulate the dictionary, we applied various type-level programming
techniques. To summarize our contributions:
\begin{itemize}
\item We present \Edis{}, a statically typed domain-specific language embedded in Haskell (with extension provided by the Glasgow Haskell Compiler) and built on \Hedis{}. Serializable Haskell datatypes are
automatically converted before being written to \Redis{} data store. Available
keys and their types are kept track of in type-level dictionaries. The types of
embedded commands state clearly their preconditions and postconditions on the
available keys and types, and a program is allowed to be constructed only if
it is guaranteed not to fail with a type error, assuming that it is the only client accessing the store.
%
\item We demonstrate the use of various type-level programming techniques,
including data kinds, singleton types and proxies, closed type families, etc.,
to define type-level lists and operations that observe and manipulate the lists.
%
\item This is (yet another) example of encoding effects and constraints of programs in types, using indexed monad~\cite{indexedmonad}, closed
type-families~\cite{tfclosed} and constraint kinds~\cite{constraintskinds}.
\end{itemize}

In Section~\ref{sec:indexed-monads} we introduce indexed monads, to reason about
pre and postconditions of stateful programs, and in Section~\ref{sec:type-level-dict} we review the basics of type-level
programming in Haskell that allows us to build type-level dictionaries to keep
track of keys and types. Embeddings of \Redis{} commands are presented in
Section~\ref{sec:embedding-commands}. In Section~\ref{sec:discussions} we
discuss various issues regarding design choices, limitations of this approach,
as well as possible future works, before we review related work in
Section~\ref{sec:conclusions}.
