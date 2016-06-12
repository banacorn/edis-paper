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

Haskell maintains the distinction between values, types, and kinds: values are
categorized by types, and types are categorized by kinds. The kinds are relatively simple: |*| is the kind of all {\em lifted} types, while type
constructors have kinds such as |* -> *|, |* -> * -> *|, etc. Consider the
datatype definitions below:
\begin{spec}
data Nat = Zero | Suc Nat {-"~~,\qquad"-} data [a] = [] | a : [a] {-"~~."-}
\end{spec}
The lefthand side is usually seen as having defined a type |Nat :: *|,
and two value constructors |Zero :: Nat| and |Suc :: Nat -> Nat|. The righthand
side is how Haskell lists are understood. The {\em kind} of |[.]| is |* -> *|,
since it takes a lifted type |a| to a lifted type |[a]|. The two value constructors respectively have types |[] :: [a]| and |(:) :: a -> [a] ->
[a]|, for all type |a|.

The GHC extension \emph{data kinds}~\cite{promotion}, however, automatically
promotes certain ``suitable'' types to kinds.\footnote{It is only informally
described in the GHC manual what types are ``suitable''.} With the extension,
the |data| definitions above has an alternative reading: |Nat| is a new kind,
|Zero :: Nat| is a type having kind |Nat|, and |Suc :: Nat -> Nat| is a type
constructor, taking a type in kind |Nat| to another type in |Nat|. Whether a
constructor is promoted can often be inferred from the context. To be more specific, prefixing a constructor with a single quote, such as in |ZERO| and
|SUC|, denotes that it is promoted.

The situation of lists is similar: for all kind |k|, |[k]| is also a kind. For
all kind |k|, |[] :: [k]| is a type. Given a type |x|
of kind |k| and a type |xs| of kind |[k]|, |x : xs| is again a type of
kind |[k]|. Formally, |(:) :: k -> [k] -> [k]|. For example,
|Int :- (Char :- (Bool :- NIL))| is a type having kind |[*]| --- a list of
(lifted) types. The optional quote denotes that the constructors are promoted.
The same list can be denoted by a syntax sugar |TList (Int, Char, Bool)|.

Tuples are also promoted. Thus we may put two types in a pair to form another
type, such as in |TPar (Int, Char)|, a type having kind |(*,*)|.

Strings in Haskell are nothing but lists of |Char|s. Regarding promotion,
however, a |String| can be promoted to a type having kind |Symbol|. |Symbol| is
a type without a constructor: |data Symbol|,
intended to be used as a promoted kind. In the expression:
\begin{spec}
"this is a type-level string literal" :: Symbol {-"~~,"-}
\end{spec}
the string on the lefthand side of |(::)| is a type, having kind |Symbol|.

With all of these ingredients, we are ready to build our dictionaries, or
type-level associate lists:
\begin{spec}
type DictEmpty = NIL {-"~~,"-}
type Dict0 = TList (TPar ("key", Bool)) {-"~~,"-}
type Dict1 = TList (TPar ("A", Int), TPar ("B", "A")) {-"~~."-}
\end{spec}
All the entities defined above are types, where |Dict0| and |Dict1|
have kind |[(Symbol, *)]|.
