%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Motivation}
\label{sec:motivation}

All binary strings are equal, but some binary strings are more equal than others.

Although everything in Redis is essentially a binary string, these strings are treated differently. Redis supports many different
kind of data structures, such as strings, hashes, lists, etc. While
they are all encoded as binary strings before being written to the databse, most commands, much like how the C language treats a piece of data, only works with data of certain types.

\paragraph{Problem 1} The command \text{SET}, by
its definition, associates a key to a string. In the following
example, the key \text{some-string} is associated
to string \text{foo}. Subsequent calls to
\text{SADD} causes runtime errors, since the value
of \text{some-string} is not a set, but a string.

\begin{verbatim}
redis> SET some-string foo
OK
redis> SADD some-string bar
(error) WRONGTYPE Operation against a key
 holding the wrong kind of value
\end{verbatim}

\paragraph{Problem 2} Even worse, not all strings are equal!
The call \text{INCR some-string} parses the string
associated with key \text{some-string}
to an integer, increments it by one, and store it back as a string.
If the string can not be parse as an integer, a runtime error
is raised.

\begin{verbatim}
redis> SET some-string foo
OK
redis> INCR some-string
(error) ERR value is not an integer or out
 of range
\end{verbatim}

\paragraph{In Hedis} Hedis, being only a simple wrapper on top
of the TCP protocol of Redis, inherits all the problems mentioned
above. The following program yields the same error as that in
the Redis client.

\begin{spec}
program :: Redis (Either Reply Integer)
program = do
    set "some-string" "foo"
    sadd "some-string" ["a"]
\end{spec}
\begin{spec}
Left (Error "WRONGTYPE Operation against a
 key holding the wrong kind of value")
\end{spec}

\paragraph{The Cause} Every key is associated with a value, and every value has
 it's own type. But most commands in Redis only work with a certain type of
 value. When a command is used on a wrong type of key, a runtime error occurs.
 The problems illustrated above arise from the absence of type checking, with
 respects to \textbf{the type of a value that associates with a key}.
 These problems could have been avoided, if we could know the type every key
 associates with in advance, and prevent programs with invalid commands from
 executing.

\subsection{Hedis as an embedded DSL}

Haskell makes it easy to build and use Domain Specific Languages (DSLs),
 and Hedis can be regarded as one of them. What makes Hedis peculiar is that,
 it has \emph{variable bindings} (between keys and values), but with very
 little or no semantic checking, neither dynamically nor statically.

We began with making Hedis a dynamically typechecked embedded DSL, and implemented a
 runtime type checker that keeps track of types of all the variable. But then we
 found that things can be a lot easier, by leveraging the host language's type
 checker. We encode variable bindings with \emph{type-level lists} and
 \emph{strings}, and control the effects on the bindings with
 \emph{indexed monad}. In contrast to the former approach, we \textbf{embedded}
 our type checker into Haskell's type system, without having to build a
 \textbf{standalone} one on the term level.

\subsection{Contributions}

To summarize our contributions:

\begin{itemize}
\item We make Hedis statically type-checked, without runtime overhead.
\item We demonstrates how to model variable bindings of an embedded DSL with
 language extensions like type-level literals and data kinds.
\item We provide (yet another) an example of encoding effects and constraints of
 an action in types, with indexed monad\cite{indexedmonad} and other language
 extensions such as closed type-families\cite{closedtypefamilies} and
 constraints kinds\cite{constraintskinds}.
\item Popcorn, a package we built for programmers. This package helps programmers
 to write more reliable Redis programs, and also makes Redis polymorphic by
 automatically converting back and forth from values of arbitrary types and
 boring ByteStrings.
\end{itemize}
