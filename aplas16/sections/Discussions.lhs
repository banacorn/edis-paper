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
get  :: (KnownSymbol k, Serialize a, StringOrNX xs k)
     => Proxy k -> Edis xs xs (EitherReply (Maybe a)) {-"~~."-}
\end{spec}
where |StringOrNX| is defined in Figure~\ref{fig:xxxOrNX}.

The problem with such typing, however, is that |a| cannot be inferred from |xs|
and |k| when |k| does not appear in |xs|. In such situations, to avoid Haskell
complaining about ambiguous type, |a| has to be specified by the caller of
|get|. The user will then be forced to spell out the complete type signature,
only to make |a| explicit.

We think it is more reasonable to enforce that, when |get| is called, the key
should exist in the data store. Thus |get| in \Redis{} has the following type:
\begin{spec}
get  :: (KnownSymbol k, Serialize a, Just (StringOf a) ~ Get xs k)
     => Proxy k -> Edis xs xs (EitherReply (Maybe a)) {-"~~,"-}
\end{spec}
which requires that |(k,a)| presents in |xs| and thus |a| is inferrable from
|xs| and |k|.

\paragraph{Commands with Variable Numbers of Input/Outputs.} Recall that, in
Section~\ref{sec:proxy-key}, the \Redis{} command \texttt{DEL} takes a variable
number of keys, while our \Edis{} counterpart takes only one. Some \Redis{}
commands take a variable number of arguments as inputs, and some returns
multiple results. Most of them are accurately implemented in \Hedis{}. For another example of a variable-input command, |sinter| in \Hedis{}, whose type
is shown below:
\begin{spec}
Hedis.sinter :: [ByteString] -> Redis (EitherReply [ByteString]) {-"~~,"-}
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
sinter  :: (  KnownSymbol k1, KnownSymbol k2, Serialize a,
              JUST (SetOf a) ~ Get xs k1, SetOrNX xs k2)
        => Proxy k1 -> Proxy k2 -> Edis xs xs (EitherReply [a]) {-"~~."-}
\end{spec}
\todo{Is this right? Should we be sure that |k2| has type |SetOf a|?}

The function |hgetall| in \Hedis{} implements the \Redis{} command \texttt{HGETALL} and has the following type:
\begin{spec}
Hedis.hgetall :: ByteString -> Redis (EitherReply [(ByteString, ByteString)]) {-"~~."-}
\end{spec}
It returns all the field-value pairs associated to a key in a hash table. Values
of different fields usually have different types. A proper implementation of
this function in \Redis{} should return a \emph{heterogeneous list}~\cite{hetero} --- a list whose elements can be of different types. We
also leave such functions as a future work.

\paragraph{Not All Safe Redis Programs Can Be Typechecked.}
Enforcing a typing discipline rules out some programs that are likely to be erroneous, and reduces the number of programs that are allowed. Like all type
systems, our type system takes a conservative estimation: there are bound to be
some \Redis{} programs not are not typable in our type system, but do not
actually throw a type error. We demand that elements in our lists must be
of the same type, for example, while a \Redis{} program could store in a list
different types of data, encoded as strings, and still works well.

\todo{no dynamic generation of keys}

\paragraph{Transactions.} Commands in \Redis{} can be wrapped in
\emph{transactions}. \Redis{} offers two promises regarding commands in a
transaction. Firstly, all commands in a transaction are serialized and
executed sequentially, without interruption from another client. Secondly,
either all of the commands or none are processed.

Support of transactions in \Edis{} is a future work.
We expect that there would not be too much difficulty --- in an early
experiment, we have implemented a runtime type checker specifically targeting
\Redis{} transactions, and we believe that the experience should be applicable
to static type checking as well.
