%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Indexed Monads}
\label{sec:indexed-monads}

Stateful computations are often reasoned using Hoare logic. A {\em Hoare triple}
$\{P\} S \{Q\}$ denotes such a proposition: if the statement $S$ is executed in
a state satisfying prediate $P$, when it terminates, the state must satisfy
predicate $Q$. Predicates $P$ and $Q$ are respectively is called the
\emph{precondition} and the \emph{postcondition} of the Hoare triple.

In Haskell, stateful computations are represented by monads. In order to
reason about their behaviors within the type system, we wish to label a state
monad with its pre and postcondition. An \emph{indexed monad}~%
\cite{indexedmonad} (also called \emph{monadish} or \emph{parameterised monad})
is a monad that, in addition to the type of value it computes, takes two more
type arguments representing an initial state and a final state, to be
interpreted like a Hoare triple~\cite{kleisli}:
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
Indexed monads have been used ~\cite{typefun,staticresources} ... \todo{for what? Some discriptions here to properly cite them.}

We define a new indexed monad |Popcorn| which, at term level, merely wraps
|Redis| in an additional constructor. The purpose is to add the
pre/postconditions at type level:
\begin{spec}
newtype Popcorn p q a = Popcorn { unPopcorn :: Redis a } {-"~~,"-}

instance IMonad Popcorn where
    unit = Popcorn . return
    bind m f = Popcorn (unPopcorn m >>= unPopcorn . f ) {-"~~."-}
\end{spec}
At term level, the |unit| and |bind| methods are not interesting: they merely
make calls to |return| and |(>>=)| of |Redis|, and extracts and re-apply the constructor |Popcorn| when necessary. With |Popcorn| being a |newtype|, they
can be optimized away in runtime. The interesting bits happen in compile type,
on the added type information.

The properties of the state we care about are the set of currently allocated
keys and their associated types. We will present, in Section~\ref{sec:type-level-dict}, techniques that allow us to specify
properties such as ``the keys in the database are |"A"|, |"B"|, and |"C"|,
respectively associated to values of type |Int|, |Char|, and |Bool|.''
For now, however, let us look at the simplest \Redis{} command.

The command \text{PING} in \Redis{} does nothing but replies a message
\text{PONG} if the connection is alive. In \Hedis{}, |ping| has type
|Redis (Either Reply Status)|. The \Popcorn{} version of |ping| simply
applys an additional constructor (functions from \Hedis{} are qualified with
|Hedis| to prevent name clashing):
\begin{spec}
ping :: Popcorn xs xs (Either Reply Status)
ping = Popcorn Hedis.ping {-"~~."-}
\end{spec}
Since |ping| does not alter the database, the postcondition and precondition
are the same. Commands that are more interesting will be introduced after
we present our type-level encoding of constraints on states.
