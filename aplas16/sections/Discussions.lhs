%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Disscussions}
\label{sec:discussions}

% \paragraph{Syntax}
% No one could ignore the glaring shortcoming of the syntax, which occurs mainly
%  in two places: \emph{symbol singletons} and \emph{indexed monad}. We are hoping
%  that these issues could be resolved with future syntactic extensions.

\paragraph{Returning Inferable Types.} \texttt{GET} is yet another command that
is invokable only under a ``well-typed or non-existent'' precondition,
mentioned in Section~\ref{sec:disjunctive-constraints}. It fetches the value of
a key and, if the key does not exist, returns a special value \texttt{nil}. An
error is raised if the value is not a string. In \Edis{} the situation is made
slightly complex, since we parse the string to the type it was supposed to have
encoded from. The \Edis{} version of |get| could be typed:
\begin{spec}
get  :: (KnownSymbol s, Serialize x, StringOrNX xs s)
     => Proxy s -> Edis xs xs (Either Reply (Maybe x)) {-"~~."-}
\end{spec}
where |StringOrNX| is defined in Figure~\ref{fig:xxxOrNX}.

The problem with such typing, however, is that |x| cannot be inferred from |xs|
and |s| when |s| does not appear in |xs|. In such situations, to avoid Haskell
complaining about ambiguous type, |x| has to be specified by the caller of
|get|. The user will then be forced to spell out the complete type signature,
only to make |x| explicit.

We think it is more reasonable to enforce that, when |get| is called, the key
should exist in the data store. Thus |get| in \Redis{} has the following type:
\begin{spec}
get  :: (KnownSymbol s, Serialize x, Just (StringOf x) ~ Get xs s)
     => Proxy s -> Edis xs xs (Either Reply (Maybe x)) {-"~~,"-}
\end{spec}
which requires that |(s,x)| presents in |xs| and thus |x| is inferrable from
|xs| and |s|.

\paragraph{Commands with Variable Numbers of Input/Outputs.} Recall that, in
Section~\ref{sec:proxy-key}, the \Redis{} command \texttt{DEL} takes a variable
number of keys, while our \Edis{} counterpart takes only one. Some \Redis{}
commands take a variable number of arguments as inputs, and some returns
multiple results. Most of them are accurately implemented in \Hedis{}. For another example of a variable-input command, |sinter| in \Hedis{}, whose type
is shown below:
\begin{spec}
Hedis.sinter :: [ByteString] -> Redis (Either Reply [ByteString]) {-"~~,"-}
\end{spec}
takes a list of keys, values of which are all supposed to be sets, and computes
their intersection (the returned list is the intersected set).

In \Edis{}, for a function to accept a list of keys as input, we have to
specify that all the keys are in the class |KnownSymbol|. This is possible --
we may define a datatype, indexed by the keys, serving as a witness that they
are all in |KnownSymbol|. We currently have not implemented such feature and
leave it as a possible future work. For now, we offer commands that take fixed
numbers of inputs. The \Edis{} counter part of |sinter| is:
\begin{spec}
sinter  :: (  KnownSymbol s, KnownSymbol t, Serialize x,
              JUST (SetOf x) ~ Get xs s, SetOrNX xs s)
        => Proxy s -> Proxy t -> Edis xs xs (Either Reply [x]) {-"~~."-}
\end{spec}

The function |hgetall| in \Hedis{} implements the \Redis{} command \texttt{HGETALL} and has the following type:
\begin{spec}
Hedis.hgetall :: ByteString -> Redis (Either Reply [(ByteString, ByteString)]) {-"~~."-}
\end{spec}
It returns all the field-value pairs associated to a key in a hash table. Values
of different fields usually have different types. A proper implementation of
this function in \Redis{} should return a \emph{heterogeneous list}~\cite{hetero} --- a list whose elements can be of different types. We
also leave such functions as a future work.

\paragraph{Not All Safe Redis Programs Can Be Typechecked.} (even if they
 might turn out to be type safe). We opted for type safety rather than
 expressiveness.
 \todo{more here.}

\paragraph{Transactions.} Commands in \Redis{} can be wrapped in
\emph{transactions}. \Redis{} offers two promises regarding commands in a
transaction. Firstly, all commands in a transaction are serialized and
executed sequentially, without interruption from another client. Secondly,
either all of the commands or none are processed.

At this point of writing, transactions are yet not supported in \Edis{}.
We expecting that there would not be too much difficulty --- in an early
experiment, we have implemented a runtime type checker specifically targeting
\Redis{} transactions, and we believe that the experience should be applicable
to static type checking as well.

\todo{|RedisTx Queued|}
