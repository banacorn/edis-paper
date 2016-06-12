%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Introduction}
\label{sec:introduction}

\Redis{}\footnote{\url{https://redis.io}} is an open source, in-memory data structure store, often used as database, cache and message broker. A \Redis{} datatype can be think of as a set of key-value pairs, where each value is associated with a binary-safe string key to identify and manipulate with.
\Redis{} allows values of various types, including strings, hashes, lists, and sets, etc, to be stored, and provides a collection of of atomic \emph{commands} to manipulate these values.

For an example, consider the following sequence of commands, entered through the interactive interface of \Redis{}. The keys \texttt{some-set} and \texttt{another-set}
are both associated to a set. The two call to command \texttt{SADD} respectively
adds three and two values to the two sets, before \texttt{SINTER} takes their intersection:
\begin{verbatim}
redis> SADD some-set a b c
(integer) 3
redis> SADD another-set a b
(integer) 2
redis> SINTER some-set another-set
1) "a"
2) "b"
\end{verbatim}

Note that the keys \texttt{some-set} and \texttt{another-set}, if not existing before the call to \texttt{SADD}, are created on site. The call to
\texttt{SADD} returns the size of the set after completion of the command.

Many third party libraries provide interfaces that allow general purpose programming languages to access \Redis{} through its TCP protocol.
For Haskell, the most popular library is \Hedis{}\footnote{\url{https://hackage.haskell.org/package/hedis}}.
The following program implements the previous example:
\begin{spec}
program :: Redis (Either Reply [ByteString])
program = do
    sadd "some-set" ["a", "b"]
    sadd "another-set" ["a", "b", "c"]
    sinter ["some-set", "another-set"] {-"~~."-}
\end{spec}
The function |sadd :: ByteString -> [ByteString] -> Redis (Either Reply Integer)| takes a key and a list of values as arguments, and returns
an |Integer| on success, or returns a |Reply|, a low-level representation of
replies from the Redis server, in case of failures. All wrapped in the monad
|Redis|, the context of command execution.\footnotemark

Note that keys and values, being nothing but binary strings in Redis, are
represented using Haskell |ByteString|. Values of other types must be encoded
as |ByteString|s before being written to the database, and decoded after being
read back.

\footnotetext{\Hedis{} provides another kind of context, |RedisTx|, for \emph{transactions}, united with |Redis| under the class |RedisCtx|. We
demonstrate only |Redis| in this paper.}

%\paragraph{Motivation}
%All binary strings are equal, but some binary strings are more equal than others.
%While everything in \Redis{} is essentially a binary string, these strings
%are treated differently.
\Redis{} supports many different kind of data
structures, such as strings, hashes, lists, etc. While they are all encoded as
binary strings before being written to the database, most commands only works
with data of certain types. In the following example, the key
\texttt{some-string} is associated to string \texttt{foo} --- the command
\texttt{SET} always associates a key to a string. The subsequent call to \texttt{SADD}, which adds a value to a set, thus causes a runtime error.
\begin{verbatim}
redis> SET some-string foo
OK
redis> SADD some-string bar
(error) WRONGTYPE Operation against a key holding the wrong
kind of value
\end{verbatim}

%\paragraph{Example 2} Even worse, not all strings are equal!
% The call \texttt{INCR some-string} parses the string associated with key
% \texttt{some-string} to an integer, increments it by one, and store it back as
% a string. If the string can not be parse as an integer, a runtime error
% is raised.
% \begin{verbatim}
% redis> SET some-string foo
% OK
% redis> INCR some-string
% (error) ERR value is not an integer or out of range
% \end{verbatim}

Being a simple wrapper on top of the TCP protocol of \Redis{}, \Hedis{}
inherits the problem. Executing following program yields the same error
wrapped in Haskell: |Left (Error| \texttt{"WRONGTYPE Operation against a
key holding the wrong kind of value"}|)|.
\begin{spec}
program :: Redis (Either Reply Integer)
program = do
    set "some-string" "foo"
    sadd "some-string" ["a"] {-"~~."-}
\end{spec}

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
Such a programming model is certainly very error-prone. Working within Haskell,
a host language with a strong typing system, one naturally wishes to build a
a domain-specific embedded language (DSEL) that exploits the rich type system
of Haskell to ensure absence of \Redis{} type errors.

This paper discusses the techniques we used and experiences we learned from building such a language, nicknamed \Edis{}. We constructed an {\em indexed
monad}, on top of the monad |Redis|, which is indexed by a dictionary that
maintains the set of currently defined keys and their types. To represent
the dictionary, we need to encode variable binds with {\em type-level} lists
and strings. To summarize our contributions:
\begin{itemize}
\item We present \Edis{}, a statically typed domain-specific language embedded in Haskell and built on \Hedis{}.
% also makes Redis polymorphic by automatically converting back and forth from values of arbitrary types and boring ByteStrings.
%
\item We demonstrate how to model variable bindings of an embedded DSL using
 language extensions including type-level literals and data kinds.
%
\item We provide (yet another) an example of encoding effects and constraints of
in types, with indexed monad~\cite{indexedmonad}, closed type-families~\cite{closedtypefamilies} and constraints kinds~\cite{constraintskinds}.
\end{itemize}
\todo{Phrase this better.}
