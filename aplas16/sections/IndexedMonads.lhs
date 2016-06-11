%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Indexed Monads}
\label{sec:indexed-monads}

Redis commands are \emph{actions}.
 We could capture the effects caused by an action, by expressing the states it
 affects, before and after. That is, the \emph{preconditions} and
 \emph{postconditions} of an action. In such way, we could also impose
 constraints on the preconditions.

\emph{Indexed monads} (or \emph{monadish},
 \emph{parameterised monad})\cite{indexedmonad}
 can be used\cite{typefun,staticresources} to model such preconditions and
 postconditions in types. An indexed monad is a type constructor that takes
 three arguments: an initial state, a final state, and the type of a value that
 an action computes, which can be read like a Hoare triple\cite{kleisli}.

\begin{spec}
class IMonad m where
    unit :: a -> m p p a
    bind :: m p q a -> (a -> m q r b) -> m p r b
\end{spec}

Class |IMonad| comes with two operations: |unit| for identities and |bind| for
compositions, as in monads.

We define a new datatype |Popcorn|, which is basically just
 the context |Redis| indexed with more information in types.
 We make it an instance of |IMonad|.

\begin{spec}
newtype Popcorn p q a =
    Popcorn { unPopcorn :: Redis a }

instance IMonad Popcorn where
    unit = Popcorn . return
    bind m f =
        Popcorn (unPopcorn m >>= unPopcorn . f )
\end{spec}

The first and second argument of type |Popcorn| is where the
dictionaries going to be.

To execute a |Popcorn| program, simply apply it to
 |unPopcorn| to get an ordinary Hedis program back,
 with type information erased.

\subsection{\text{PING}: the first attempt}

In Redis, \text{PING} does nothing but replies with
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
