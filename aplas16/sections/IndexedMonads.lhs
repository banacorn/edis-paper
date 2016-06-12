%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Indexed Monads}
\label{sec:indexed-monads}

Stateful computations are often reasoned in a Hoare-logic style: each command
is labelled by a \emph{precondition} and a \emph{postcondition}. If the former
is satisfied before the command is executed, the latter is guaranteed to hold
afterwards.

In Haskell, stateful computations are represented by monads. In order to
reason about their behaviors within the type system, we wish to label a state
monad with its pre and postcondition. An \emph{indexed
monad}~\cite{indexedmonad} (also called \emph{monadish} or
\emph{parameterised monad}) is a monad that, in addition to the type of value
it computes, takes two more type arguments representing an initial state and
a final state, to be interpreted like a Hoare triple~\cite{kleisli}:
\begin{spec}
class IMonad m where
    unit :: a -> m p p a
    bind :: m p q a -> (a -> m q r b) -> m p r b {-"~~."-}
\end{spec}
The intention is that a computation of type |m p q a| is a stateful computation
such that if it starts execution in a state satisfying |p| and terminates, it
yields a value of type |a|, and the new state satisfies |q|.
The operator |unit| lifts a pure computation to a stateful computation that
does not alter the state. In |x `bind` f|, a computation |x :: m p q a| must
be chained before |f :: a -> m q r b|, which expects a value of type |a| and
a state satisfying |q| and, if terminates, ends in a state satisfying |r|.
The result is a monad |m p r b| --- a computation that, if executed in a state
satisfying |p| and terminates, yields a value |b| and a state satisfying |r|.
Indexed monads have been used ~\cite{typefun,staticresources} ... \todo{for what? Some discriptions here to properly cite them.}

We define a new indexed monad |Popcorn| which, at term level, merely wraps
|Redis| in an additional constructor. The purpose is to add the pre and
postconditions at type level:
\begin{spec}
newtype Popcorn p q a = Popcorn { unPopcorn :: Redis a } {-"~~,"-}

instance IMonad Popcorn where
    unit = Popcorn . return
    bind m f = Popcorn (unPopcorn m >>= unPopcorn . f ) {-"~~."-}
\end{spec}
To execute a |Popcorn| program, simply apply it to |unPopcorn| to erase the additional type information and get back an ordinary \Hedis{} program.

\paragraph{\text{PING}: A First Example}
In \Redis{}, \text{PING} does nothing but replies with
 \text{PONG} if the connection is alive. In Hedis,
 |ping| has type:

\begin{spec}
ping :: Redis (Either Reply Status)
\end{spec}

Now with |Popcorn|, we could make our own version of
|ping|\footnotemark

\footnotetext{|ping| from Hedis is qualified with
|Hedis| to prevent function name clashing in our code.}

\begin{spec}
ping :: Popcorn xs xs (Either Reply Status)
ping = Popcorn Hedis.ping
\end{spec}

The dictionary |xs| in the type remains unaffected after the
 action, because |ping| does not affect any key-type
 bindings. To encode other commands that modifies key-type bindings, we need
 type-level functions to annotate those effects on the dictionary.
