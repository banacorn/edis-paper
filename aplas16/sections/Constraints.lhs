%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Imposing constraints}
\label{sec:constraints}

To rule out programs with undesired properties, certain constraints must be
imposed, on what arguments they can take, or what preconditions they must hold.

Consider the following example: \texttt{LLEN} returns the length of
 the list associated with a key, else raises a type error.

\begin{verbatim}
redis> LPUSH some-list bar
(integer) 1
redis> LLEN some-list
(integer) 1
redis> SET some-string foo
OK
redis> LLEN some-string
(error) WRONGTYPE Operation against a key
holding the wrong kind of value
\end{verbatim}

Such constraint could be expressed in types with
\emph{equality constraints}\cite{typeeq}.

\begin{spec}
llen :: (KnownSymbol s
      , Get xs s ~ Just (ListOf x))
     => Proxy s
     -> Edis xs xs (Either Reply Integer)
llen key =
     Edis $ Hedis.llen (encodeKey key)
\end{spec}

Where |(~)| denotes that |Get xs s|
and |Just (ListOf x)| needs to be the same.

The semantics of \text{LLEN} defined above is actually not
complete. \text{LLEN} also accepts keys that do not exist, and
 replies with \text{0}.

\begin{verbatim}
redis> LLEN nonexistent
(integer) 0
\end{verbatim}

In other words, we require that the key to be associated with a list,
 \textbf{unless} it doesn't exist at all.

\subsection{Expressing Constraint Disjunctions}

Unfortunately, expressing disjunctions in constraints is much more difficult
 than expressing conjunctions, since the latter could be easily done by placing
 constraints in a tuple (at the left side of |=>|).

There are at least three ways to express type-level constraints
\cite{singletons}. Luckily we could express constraint disjunctions with type
 families in a modular way.

The semantics we want could be expressed informally like this:
|Get xs s| $\equiv$ |Just (ListOf x)|
$\vee$ $\neg$ |(Member xs s)|

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
