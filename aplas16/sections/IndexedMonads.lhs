%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Indexed Monads}
\label{sec:indexed-monads}

Stateful computations are often reasoned using Hoare logic. A {\em Hoare triple}
$\{P\} S \{Q\}$ denotes the following proposition: if the statement $S$ is
executed in a state satisfying prediate $P$, when it terminates, the state must
satisfy predicate $Q$. Predicates $P$ and $Q$ are respectively called the
\emph{precondition} and the \emph{postcondition} of the Hoare triple.

In Haskell, stateful computations are represented by monads. In order to
reason about their behaviors within the type system, we wish to label a state
monad with its pre and postcondition. An \emph{indexed monad}~%
\cite{indexedmonad} (also called \emph{parameterised monad} or \emph{monadish})
is a monad that, in addition to the type of its result, takes two more
type arguments representing an initial state and a final state, to be
interpreted like a Hoare triple:
\begin{spec}
class IMonad m where
    unit :: a -> m p p a
    bind :: m p q a -> (a -> m q r b) -> m p r b {-"~~."-}
\end{spec}
The intention is that a computation of type |m p q a| is a stateful computation
such that, if it starts execution in a state satisfying |p| and terminates, it
yields a value of type |a|, and the new state satisfies |q|. The operator |unit|
lifts a pure computation to a stateful computation that does not alter the
state. In |x `bind` f|, a computation |x :: m p q a| is followed by
|f :: a -> m q r b| --- the postcondition of |x| matches the precondition of
the computation returned by |f|. The result is a monad |m p r b|.

We define a new indexed monad |Edis|. At term level, the |unit| and |bind|
methods are not interesting: they merely make calls to |return| and |(>>=)| of
|Redis|, and extracts and re-apply the constructor |Edis| when necessary.
With |Edis| being a |newtype|, they can be optimized away in runtime. The
purpose is to add the pre/postconditions at type level:
\begin{spec}
newtype Edis p q a = Edis { unEdis :: Redis a } {-"~~,"-}

instance IMonad Edis where
    unit = Edis . return
    bind m f = Edis (unEdis m >>= unEdis . f ) {-"~~."-}
\end{spec}

The properties of the state we care about are the set of currently
allocated keys and types of their values. We will present, in Section~\ref{sec:type-level-dict}, techniques that allow us to specify
properties such as ``the keys in the database are |"A"|, |"B"|, and |"C"|,
respectively assigned values of type |Int|, |Char|, and |Bool|.'' For now,
however, let us look at the simplest \Redis{} command.

\Redis{} commands can be executed in two contexts: normal, and in a \emph{transaction}. In \Hedis{}, a command yielding value of type |a| in the
normal case is represented by |Redis (Either Reply a)|, as mentioned in
Section~\ref{sec:introduction}; in a transaction, the command is represented by
two other datatypes |RedisTx (Queued a)|. In this paper we focus on the former
case. For brevity we abbreviate |Either Reply a| to |EitherReply a|.

The command \texttt{PING} in \Redis{} does nothing but replying a message
\texttt{PONG} if the connection is alive. In \Hedis{}, |ping| has type
|Redis (EitherReply Status)|. The \Edis{} version of |ping| simply applys an additional constructor (functions from \Hedis{} are qualified with |Hedis| to
prevent name clashing):
\begin{spec}
ping :: Edis xs xs (EitherReply Status)
ping = Edis Hedis.ping {-"~~."-}
\end{spec}
Since |ping| does not alter the database, the postcondition and precondition
are the same. Commands that are more interesting will be introduced after
we present our type-level encoding of constraints on states.
