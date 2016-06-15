%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Type-Level Dictionaries}
\label{sec:type-level-dict}

One of the challenges of statically ensuring type correctness of \Redis{},
which also presents in other stateful languages, is that the type of the value
associated to a key can be altered after updating. \todo{Is it so in \Redis{}?}
To ensure type correctness, we keep track of the types of all existing keys in a {\em dictionary} --- conceptually, an associate list, or a list of pairs of keys
and some encoding of types. For example, we may want the dictionary |[("A",Int),
("B", Char), ("C", Bool)]| to represent a predicate stating that ``the keys |"A"|, |"B"|, and |"C"| are respectively associated to values of type |Int|,
|Char|, and |Bool|.''

The dictionary above mixes values (strings such as |"A"|, |"B"|) and types.
Further more, as mentioned in Section~\ref{sec:indexed-monads}, the
dictionaries will be parameters to the indexed monad |Edis|. In a
dependently typed programming language (without the so-called ``phase
distinction'' --- separation between types and terms), this would pose no
problem. In Haskell however, the dictionaries, to index a monad, has to be a
type as well.

In this section we describe how to construct a type-level dictionary, to be
used with the indexed monad in Section~\ref{sec:indexed-monads}.

\subsection{Datatype Promotion}

Haskell maintains the distinction between values, types, and kinds: values are
categorized by types, and types are categorized by kinds. The kinds are
relatively simple: |*| is the kind of all {\em lifted} types, while type
constructors have kinds such as |* -> *|, or |* -> * -> *|, etc. Consider the
datatype definitions below:
\begin{spec}
data Nat = Zero | Suc Nat {-"~~,\qquad"-} data [a] = [] | a : [a] {-"~~."-}
\end{spec}
The lefthand side is usually seen as having defined a type |Nat :: *|,
and two value constructors |Zero :: Nat| and |Suc :: Nat -> Nat|. The righthand
side is how Haskell lists are understood. The {\em kind} of |[.]| is |* -> *|,
since it takes a lifted type |a| to a lifted type |[a]|. The two value
constructors respectively have types |[] :: [a]| and |(:) :: a -> [a] ->
[a]|, for all type |a|.

The GHC extension \emph{data kinds}~\cite{promotion}, however, automatically
promotes certain ``suitable'' types to kinds.\footnote{It is only informally
described in the GHC manual what types are ``suitable''.} With the extension,
the |data| definitions above has an alternative reading: |Nat| is a new kind,
|Zero :: Nat| is a type having kind |Nat|, and |Suc :: Nat -> Nat| is a type
constructor, taking a type in kind |Nat| to another type in |Nat|. When one
sees a constructor in an expression, whether it is promoted can often be
inferred from the context. When one needs to be more specific, prefixing a
constructor with a single quote, such as in |ZERO| and |SUC|, denotes that it
is promoted.

The situation of lists is similar: for all kind |k|, |[k]| is also a kind. For
all kind |k|, |[]| is a type of kind |[k]|. Given a type |x| of kind |k| and a
type |xs| of kind |[k]|, |x : xs| is again a type of kind |[k]|. Formally,
|(:) :: k -> [k] -> [k]|. For example, |Int :- (Char :- (Bool :- NIL))| is a
type having kind |[*]| --- it is a list of (lifted) types. The optional quote
denotes that the constructors are promoted. The same list can be denoted by a
syntax sugar |TList (Int, Char, Bool)|.

Tuples are also promoted. Thus we may put two types in a pair to form another
type, such as in |TPar (Int, Char)|, a type having kind |(*,*)|.

Strings in Haskell are nothing but lists of |Char|s. Regarding promotion,
however, a |String| can be promoted to a type having kind |Symbol|. |Symbol| is
a type without a constructor: |data Symbol|, intended to be used as a promoted
kind. In the expression:
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
All the entities defined above are types, where |Dict0| has kind |[(Symbol,
*)]|. In |Dict1|, while |Int| has kind |*| and |"A"| has kind |Symbol|, the former kind subsumes the later. Thus |Dict1| also has kind |[(Symbol, *)]|.

\subsection{Type-Level Functions}
\label{sec:type-fun}

Now that we can represent dictionaries as types, the next step is to define
operations on them. A function that inserts an entry to a dictionary, for
example, is a function from a type to a type. While it was shown that it is
possible to simulate type-level functions using Haskell type
classes~\cite{McBride:02:Faking}, in recent versions of GHC, {\em indexed type
families}, or type families for short, are considered a cleaner solution.

For example, compare disjunction |(||||)| and its type-level
counterpart |Or|:\\
\noindent{\centering %\small
\begin{minipage}[b]{0.35\linewidth}
\begin{spec}
(||) :: Bool -> Bool -> Bool
True  &&  b  = True
a     &&  b  = b {-"~~,"-}
\end{spec}
\end{minipage}
\begin{minipage}[b]{0.55\linewidth}
\begin{spec}
type family Or (a :: Bool) (b :: Bool) :: Bool
  where  Or  True  b  = True
         Or  a     b  = b {-"~~."-}
\end{spec}
\end{minipage}
}\\
The lefthand side is a typical definition of |(||||)| by pattern matching.
On the righthand side, |Bool| is not a type, but a type lifted to a kind,
while |True| and |False| are types of kind |Bool|. The declaration says that
|Or| is a family of types, indexed by two parameters |a| and |b| of kind |Bool|.
The type with index |True| and |b| is |True|, and all other indices lead to |b|.
For our purpose, we can read |Or| as a function from types to types --- observe
how it resembles the term-level |(||||)|. We present two more type-level
functions about |Bool| --- negation, and conditional, that we will use later:\\
\noindent{\centering %\small
\begin{minipage}[b]{0.35\linewidth}
\begin{spec}
type family Not a where
  Not FALSE  = TRUE
  Not TRUE   = FALSE {-"~~,"-}
\end{spec}
\end{minipage}
\begin{minipage}[b]{0.55\linewidth}
\begin{spec}
type family If (c :: Bool) (t :: a) (f :: a) :: a where
  If TRUE  tru  fls = tru
  If FALSE tru  fls = fls {-"~~."-}
\end{spec}
\end{minipage}
}

As a remark, type families in Haskell come in many flavors. Families can
be defined for |data| and |type| synonym. They can appear inside type
classes~\cite{tfclass,tfsynonym} or at toplevel. Toplevel type families can be
open~\cite{tfopen} or closed~\cite{tfclosed}. The flavor we chose is top-level,
closed type synonym family, since it allows overlapping instances, and since we need none of the extensibility provided by open type families. Notice that the instance |Or True b| could be subsumed under the more general instance, |Or a
b|. In a closed type family we may resolve the overlapping in order, just like
how cases overlapping is resolved in term-level functions.

%\subsection{Functions on Type-Level Dictionaries}

We are now able to define operations on type-level dictionaries. Let's begin
with dictionary lookup.
\begin{spec}
type family Get (xs :: [(Symbol, *)]) (s :: Symbol) :: * where
    Get (TPar (s, x) :- xs) s  =  x
    Get (TPar (t, x) :- xs) s  =  Get xs s {-"~~."-}
\end{spec}
The type-level function |Get| returns the entry associated with key |s| in the
dictionary |xs|. Notice, in the first case, how type-level equality can be
expressed by unifying type variables with the same name. Note also that |Get| is
a partial function on types: while |Get (TList (TPar ("A", Int))) "A"| evaluates
to |Int|, when |Get (TList (TPar ("A", Int))) "B"| appears in a type expression,
there are no applicable rules to reduce it. The expression thus stays as it is.

We could make |Get| total, as we would at the term level, with |Maybe|:
\begin{spec}
type family Get (xs :: [(Symbol, *)]) (s :: Symbol) :: Maybe * where
    Get NIL                 s = Nothing
    Get (TPar(s, x) :- xs)  s = Just x
    Get (TPar(t, x) :- xs)  s = Get xs s {-"~~."-}
\end{spec}
%
Some other dictionary-related functions are defined in a similar fashion
in Figure \ref{fig:dict-operations}. The function |Set| either updates an
existing entry or inserts a new entry, |Del| removes an entry matching
a given key, while |Member| checks whether a given key exists in the
dictionary.

\begin{figure}[t]
\begin{spec}
-- inserts or updates an entry
type family Set  (xs :: [(Symbol, *)]) (s :: Symbol) (x :: *) :: [(Symbol, *)] where
    Set NIL                 s x = TList (TPar (s, x))
    Set (TPar(s, y) :- xs)  s x = TPar (s, x) :- xs
    Set (TPar(t, y) :- xs)  s x = TPar (t, y) :- Set xs s x

-- removes an entry
type family Del  (xs :: [(Symbol, *)]) (s :: Symbol) :: [(Symbol, *)] where
    Del Nil                 s  = Nil
    Del (TPar (s, y) :- xs) s  = xs
    Del (TPar (t, y) :- xs) s  = TPar (t, y) :- Del xs s

-- membership
type family Member (xs :: [(Symbol, *)]) (s :: Symbol) :: Bool where
    Member Nil                  s = False
    Member (TPar(s, x) :- xs)   s = True
    Member (TPar(t, x) :- xs)   s = Member xs s
\end{spec}
\caption{Some operations on type-level dictionaries.}
\label{fig:dict-operations}
\end{figure}
