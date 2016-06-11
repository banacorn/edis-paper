%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Introduction}
\label{sec:introduction}

\subsection{Redis}

Redis\footnote{\url{https://redis.io}} is an open source, in-memory data structure store, often used as
database, cache and message broker. A Redis datatype can be think of as a set of key-value pairs, where each value is associated with a binary-safe string key to identify and manipulate with. Redis supports many different kind of values, such as strings, hashes, lists, and sets, etc, and provides a collection of of atomic \emph{commands} to manipulate these values.

For an example, consider the following sequence of commands, entered through the interactive interface of Redis. The keys
\text{some-set} abd \text{another-set} are
both associated to a set. The two call to command \text{SADD} respectively adds three and two values to the two sets, before \text{SINTER} takes their intersection:
\begin{verbatim}
redis> SADD some-set a b c
(integer) 3
redis> SADD another-set a b
(integer) 2
redis> SINTER some-set another-set
1) "a"
2) "b"
\end{verbatim}

Note that the keys \text{some-set} and \text{another-set}, if not existing before the call to \text{SADD}, are created on site. The call to
\text{SADD} returns the size of the set after
completion of the command.

\subsection{Hedis}

Many third party library exist to allow general purpose programmings
to access Redis databases through its TCP protocol. The most popular
such library for Haskell is Hedis\footnote{\url{https://hackage.haskell.org/package/hedis}}.

The following program is how the previous example looks like in
Hedis:

\begin{spec}
program :: Redis (Either Reply [ByteString])
program = do
    sadd "some-set" ["a", "b"]
    sadd "another-set" ["a", "b", "c"]
    sinter ["some-set", "another-set"]
\end{spec}

The function |sadd| takes a key and a list of values as
 arguments, and returns an |Integer| on success, or
 returns a |Reply|, a low-level representation of replies
 from the Redis server, in case of failures. All wrapped in
 |Redis|\footnotemark, the context of command execution.

\begin{spec}
sadd  :: ByteString       -- key
      -> [ByteString]     -- values
      -> Redis (Either Reply Integer)
\end{spec}

Note that keys and values, being nothing but binary strings in Redis,
are represented using Haskell |ByteString|.
Values of other types must be encoded as |ByteString|s before being written to the database,
and decoded after being read back.

\footnotetext{Hedis provides another kind of context, |RedisTx|, for \emph{transactions}, united with |Redis| under the class of |RedisCtx|. For brevity, we demonstrate only |Redis| in this paper. }
