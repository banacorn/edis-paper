%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Type-Level Dictionaries}
\label{sec:type-level-dict}

One of the challenges of statically ensuring type correctness of \Redis{},
which also presents in other stateful languages, is that the type of the value
associated to a key can be altered after updating. To ensure type correctness,
we have to keep track of the (\Redis{}) types of all existing keys in a
{\em dictionary} --- conceptually, a list of pairs of keys and \Redis{} types.
Each \Redis{} command is embedded in \Popcorn{} as a monadic computation. The
monad, to be presented in Section~\ref{sec:indexed-monads}, is indexed by
the dictionaries before and after the computation. In a dependently typed
programming language (without the so-called ``phase distinction'' ---
separation between types and terms), this would pose no problem. In Haskell
however, the dictionaries, to index a monad, has to be a Haskell type as well.

In this section we describe how to construct a type-level dictionary, to be
used with the indexed monad in Section~\ref{sec:indexed-monads}. More operations
on the dictionary will be presented in Section~\ref{sec:type-level-fun}.

\subsection{Datatype Promotion}

% Normally, at the term level, we could express the datatype of dictionary with
% \emph{type synonym} like this.\footnotemark
%
% \begin{spec}
% type Key = String
% type Dictionary = [(Key, TypeRep)]
% \end{spec}
%
% \footnotetext{|TypeRep| supports term-level representations
%  of datatypes, available in |Data.Typeable|}

A datatype definition such as the one below:
\begin{spec}
data List a = Nil | Cons a (List a) {-"~~,"-}
\end{spec}
is usually understood as having defined a type constructor |List|, and two value
constructors |Nil| and |Cons|. For all lifted types |a|, |List a| is also a
lifted type. Thus the {\em kind} of |List| is |* -> *|, where |*| is the kind of
all {\em lifted types} in Haskell. The two value constructors respectively
have types |Nil :: List a| and |Cons :: a -> List a -> List a|.

The GHC extension \emph{data kinds}~\cite{promotion}, however, automatically
promotes certain ``suitable'' types to kinds. With the extension, the |data|
definition has an alternative reading: for all kind |k|, |List k| is also a
kind. |Nil| is a type, whose kind is |List k| for some |k|. Given a type |x|
of kind |k| and a type |xs| of kind |List k|, |Cons x xs| is again a type of
kind |List k|. Formally, for kind |k|, |Nil :: List k| and |Cons :: k ->
List k -> List k|. Whether a constructor is promoted can often be inferred
from the context. To be more specific, prefixing a contructor with
a single quote, such as in |pNIL| and |CONS|, denotes that it is promoted.

The built-in lists in Haskell has two constructors |[]| and |(:)|, and
|[1, 2, 3]| is take as a syntax sugar for |1 : (2 : (3 : []))|. The type and
data constructors can also be promoted. Given kind |k|, |[k]| is also a kind.
The type constructor 
For example, since |Int|, |Char|, etc., all have kind |*|, |Int :- (Char :- (Bool :- NIL))| is a type having kind |[*]| -- a list of types. The same list can be denoted by a syntax
sugar |OpenTList Int, Char, Bool CloseTList|.

sugar for |1 :- (2 :- (3 :- NIL))|
Haskell sugars lists |[1, 2, 3]| and tuples
 |(1, 'a')| with brackets and parentheses.
 We could also express promoted lists and tuples in types like this with
 a single quote prefixed. For example:
 |OpenTList Int, Char CloseTList|, |OpenTPar Int, Char CloseTPar|.

\subsection{Type-level literals}

Now we have type-level lists and tuples to construct the dictionary.
For keys, |String| also has a type-level correspondence:
|Symbol|.

\begin{spec}
data Symbol
\end{spec}

Symbol is defined without a value constructor, because it's intended to be used
 as a promoted kind.

\begin{spec}
"this is a type-level string literal" :: Symbol
\end{spec}
%
% Nonetheless, it's still useful to have a term-level value that links with a
%  Symbol, when we want to retrieve type-level information at runtime (but not the
%  other way around!).

\subsection{Putting Everything Together}

With all of these ingredients ready, let's build some dictionaries!

\begin{spec}
type DictEmpty = '[]
type Dict0 = '[ '("key", Bool) ]
type Dict1 = '[ '("A", Int), '("B", "A") ]
\end{spec}

These dictionaries are defined with \emph{type synonym}, since they are
 \emph{types}, not \emph{terms}. If we ask \text{GHCi} what is the
 kind of |Dict1|, we will get |Dict1 :: [ (Symbol, *) ]|

The kind |*| (pronounced ``star'') stands for the set of all
 concrete type expressions, such as |Int|,
 |Char| or even a symbol |"symbol"|,
 while |Symbol| is restricted to all symbols only.
