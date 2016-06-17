%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Introduction}
\label{sec:introduction}

\Redis{}\footnote{\url{https://redis.io}} is an open source, in-memory data structure store, often used as database, cache, and message broker. A \Redis{} data store can be think of as a set of key-value pairs. The value can be a
string, a list of strings, a set of strings, or a hash table of strings, etc.
However, string is the only primitive datatype. Numbers, for example, have to be
serialized to strings before being saved in the data store, and parsed back to
numbers to be manipulated with. While the concept is simple, \Redis{} is used
as an essential component in a number of popular, matured service, including
Twitter, GitHub, Weibo, StackOverflow, and Flickr, etc.

For an example, consider the following sequence of commands, entered through the interactive interface of \Redis{}. The keys \texttt{some-set} and
\texttt{another-set} are both assigned a set of strings. The two call to
command \texttt{SADD} respectively adds three and two strings to the two sets,
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
\noindent Notice that the keys \texttt{some-set} and \texttt{another-set}, if not existing before the call to \texttt{SADD}, are created on site. The calls to
\texttt{SADD} return the size of the resulting set.

Many third party libraries provide interfaces for general purpose programming
languages to access \Redis{} through its TCP protocol. For Haskell, the most
popular library is
\Hedis{}.\footnote{\url{https://hackage.haskell.org/package/hedis}}
A (normal) \Redis{} computation returning a value of type |a| is represented in
Haskell by |Redis (Either Reply a)|, where the type |Redis| is a monad, while
|Either Reply a| indicates that the computation either returns a value of type
|a|, or fails with an error message |Reply|, a representation of replies
from the \Redis{} server. The following program implements the previous example:
\begin{spec}
program :: Redis (Either Reply [ByteString])
program = do
    sadd "some-set" ["a", "b"]
    sadd "another-set" ["a", "b", "c"]
    sinter ["some-set", "another-set"] {-"~~."-}
\end{spec}
The function |sadd :: ByteString -> [ByteString] -> Redis (Either Reply Integer)| takes a key and a list of values as arguments, and returns a
\Redis{} computation yielding |Integer|. Keys and values, being nothing but
binary strings in \Redis{}, are represented using Haskell |ByteString|.

% \footnotetext{\Hedis{} provides another kind of context, |RedisTx|, for
% \emph{transactions}. We focus on |Redis| in this paper.}

\paragraph{The Problems} Most commands only works with data of certain types. In
the following example, the key \texttt{some-string} is assigned a string
\texttt{foo} --- the command \texttt{SET} always assigns a string to a key.
The subsequent call to \texttt{SADD}, which adds a value to a set, thus causes a runtime error.
\begin{Verbatim}[xleftmargin=.4in]
redis> SET some-string foo
OK
redis> SADD some-string bar
(error) WRONGTYPE Operation against a key holding
the wrong kind of value
\end{Verbatim}
\noindent For another source of type error, the command \texttt{INCR key}
incrementa the value associated to \texttt{key} by one. With strings being the
only primitive type, \Redis{} parses the stored string to an integer and, after
incrementation, stores a string back. If the string can not be parse as an
integer, a runtime error is raised.

The reader must have noticed the peculiar pattern of value creation and update
in \Redis{}: the same command is used both to create a key-value pair and to
update them. Similar to \texttt{SADD}, the command \texttt{LPUSH} appends a
value (a string) to a list, or creates one if the key does not exist:
\begin{Verbatim}[xleftmargin=.4in]
redis> LPUSH some-list bar
(integer) 1
\end{Verbatim}
\noindent Another command \texttt{LLEN} returns the length of the list, and
signals an error if the key is not associated to a list:
\begin{Verbatim}[xleftmargin=.4in]
redis> LLEN some-list
(integer) 1
redis> SET some-string foo
OK
redis> LLEN some-string
(error) WRONGTYPE Operation against a key holding
the wrong kind of value
\end{Verbatim}
\noindent Curiously, however, when applied to a key not yet created,
\Redis{} designers chose to let \texttt{LLEN} return \texttt{0}:
\begin{Verbatim}[xleftmargin=.4in]
redis> LLEN nonexistent
(integer) 0
\end{Verbatim}

Being a simple wrapper on top of the TCP protocol of \Redis{}, \Hedis{}
inherits all the behaviors. Executing following program yields the same error
wrapped in Haskell: |Left (Error| \texttt{"WRONGTYPE Operation against a
key holding the wrong kind of value"}|)|.
\begin{spec}
program :: Redis (Either Reply Integer)
program = do
    set "some-string" "foo"
    sadd "some-string" ["a"] {-"~~."-}
\end{spec}

Such a programming model is certainly very error-prone. Working within Haskell,
a host language with a strong typing system, one naturally wishes to build a
domain-specific embedded language (DSEL) that exploits the rich type system
of Haskell to not only ensure absence of \Redis{} type errors, but also provides
better documentation. We wish to be sure that a program calling \texttt{INCR},
for example, can be type checked only if we can statically guarantee that the
value to be accessed is indeed an integer. We wish to see from the type of
operators such as \texttt{LLEN} when it can be called, and allow it to be used
only in contexts that are safe. We may even want to explicitly declare existence
of certain keys in the data store and, when we are done with them, renounce them
to prevent further access, as well as preventing possible errors.

% \paragraph{The Cause} Every key is associated with a value, and every value has
% its own type. But most commands in \Redis{} only work with a certain type of
%  value. When a command is used on a wrong type of key, a runtime error occurs.
%  The problems illustrated above arise from the absence of type checking, with
%  respects to \textbf{the type of a value that associates with a key}.
%  These problems could have been avoided, if we could know the type every key
%  associates with in advance, and prevent programs with invalid commands from
%  executing.
%
% \paragraph{Hedis as an embedded DSL}
% Haskell makes it easy to build and use {\em domain specific embedded languages} (DSELs), and \Hedis{} can be regarded as one of them. What makes \Hedis{} peculiar is that,
%  it has \emph{variable bindings} (between keys and values), but with very
%  little or no semantic checking, neither dynamically nor statically.
%

This paper discusses the techniques we used and experiences we learned from building such a language, nicknamed \Edis{}. We constructed an {\em indexed
monad}, on top of the monad |Redis|, which is indexed by a dictionary that
maintains the set of currently defined keys and their types. To represent
the dictionary, we need to encode variable bindings with {\em type-level} lists
and strings. And to manipulate the dictionary, we applied various type-level
programming techniques. To summarize our contributions:
\begin{itemize}
\item We present \Edis{}, a statically typed domain-specific language embedded in Haskell and built on \Hedis{}. Serializable Haskell datatypes are
automatically converted before being written to \Redis{} data store. Available
keys and their types are kept track of in type-level dictionaries. The types of
embedded commands state clearly their preconditions and postconditions on the
available keys and types, and a program is allowed to be constructed only if
it is guaranteed not to fail with a type error.
%
\item We demonstrate the use of various type-level programming techniques,
including data kinds, singleton types and proxies, closed type families, to
define type-level lists and operation that observes and manipulates the lists.
%
\item This is (yet another) example of encoding effects and constraints of programs in types, using indexed monad and constraint kinds.
%\cite{indexedmonad}, closed type-families~\cite{closedtypefamilies} and constraints kinds~\cite{constraintskinds}.
\end{itemize}

In Section~\ref{sec:indexed-monads} we introduce indexed monads, to reason about
pre and postconditions of stateful programs, and in Section~\ref{sec:type-level-dict} we review the basics of type-level
programming in Haskell that allows us to build type-level dictionaries to keep
track of keys and types. Embeddings of \Redis{} commands are presented in
Section~\ref{sec:embedding-commands}. In Section~\ref{sec:discussions} we
discuss various issues regarding design choices, limitations of this approach,
as well as possible future works, before we review related work in
Section~\ref{sec:conclusions}.
