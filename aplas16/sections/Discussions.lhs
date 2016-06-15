%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Disscussions}
\label{sec:discussions}

% \paragraph{Syntax}
% No one could ignore the glaring shortcoming of the syntax, which occurs mainly
%  in two places: \emph{symbol singletons} and \emph{indexed monad}. We are hoping
%  that these issues could be resolved with future syntactic extensions.

\paragraph{Returning only determined datatypes}
Many commands in Redis are invokable under preconditions similar to the
``list or nothing'' constraint mentioned in
Section~\ref{sec:disjunctive-constraints}. Consider \texttt{GET}, which fetches
a string associated with a key and should be typed:
\begin{spec}
get :: (KnownSymbol s, Serialize x, StringOrNX xs s)
    => Proxy s -> Edis xs xs (Either Reply (Maybe x)) {-"~~."-}
\end{spec}
The constraint |StringOrNX| is defined by
\begin{spec}
type StringOrNX xs s =
  (IsString (FromJust (Get xs s)) `Or` Not (Member xs s)) ~ TRUE {-"~~,"-}
\end{spec}
where |IsString (StringOf n)| is |TRUE| for all type |n|, and |IsString x|
is |FALSE| otherwise.
% type family IsString (x :: *) :: Bool where
%     IsString (StringOf n) = 'True
%     IsString x            = 'False

Since the key might not exist, we don't know what |x| would
be. We could left |x| ambiguous, and let it be decided by
 the caller. But users will then be forced to spell out the complete
type signature of everything, including the dictionaries, only to specify
the desired resulting type.

Instead of allowing the key to be non-existent, we require that the key must
 exist and it's associating type to be determined at compile time. So our
 version of |get| has a stricter semantics:

\begin{spec}
get  :: (KnownSymbol s, Serialize x, Just (StringOf x) ~ Get xs s)
     => Proxy s -> Edis xs xs (Either Reply (Maybe x)) {-"~~."-}
\end{spec}

\paragraph{Commands with multiple inputs or outputs}

Some command may take a variable number of arguments as inputs, and returns more
 than one value as outputs. To illustrate this, consider
 |sinter| in Hedis:

\begin{spec}
Hedis.sinter :: [ByteString]                      -- keys
             -> Redis (Either Reply [ByteString]) -- values
\end{spec}

In Hedis such command could easily be expressed with lists of
 |ByteString|s. But in \Edis{}, things escalate quickly, as
 the keys and values will have to be expressed with \emph{heterogeneous
 lists}\cite{hetero}, which would be pratically infeasible, considering the cost,
 if not impossible.

Most importantly, the keys will all have to be constrained by
 |KnownSymbol|, which enforces these type literals to be
 concrete and known at compile time.
 It's still unclear whether this is possible.

So instead, we are offering commands that only has a single input and output.

\begin{spec}
sinter :: ByteString                      -- single key
       -> Redis (Either Reply ByteString) -- single value
\end{spec}

\textbf{Not all Redis programs can be typechecked} (even if they
 might turn out to be type safe). We opted for type safety rather than
 expressiveness.

\paragraph{Redis Transactions}

Redis has \emph{transactions}, another context for executing commands.
Redis transactions are atomic in the sense that, all commands in a transaction
 will executed sequentially, and no other requests issued by other clients will
 be served \textbf{in the middle}.\footnotemark In contrast, we cannot make such
 a guarantee in the ordinary context, which may destroy the assertions we made
 in types.

At this point of writing, transactions are not supported in our implementation.
 We are planning to add it in the future, and we are expecting that there
 wouldn't be much difficulty, since we've implemented a runtime type checker
 specifically targeting Redis transactions once, before we moved on to the
 types.
