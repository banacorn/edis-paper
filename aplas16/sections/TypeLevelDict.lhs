%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Type-Level Dictionaries}
\label{sec:type-level-dict}

One of the challenges of statically ensuring type correctness of stateful
languages is that the type of the value of a key can be altered by updating.
In \Redis{}, one may delete an existing key and create it again by assigning to
it a value of a different type. To ensure type correctness, we keep track of the
types of all existing keys in a {\em dictionary} --- an associate list, or a
list of pairs of keys and some encoding of types. For example, we may use the
dictionary |[("A",Int), ("B", Char), ("C", Bool)]| to represent a
predicate, or a constraint, stating that ``the keys in the data store are |"A"|,
|"B"|, and |"C"|, respectively assigned values of type |Int|, |Char|, and
|Bool|.'' (This representation will be refined in the next section.)

The dictionary above mixes values (strings such as |"A"|, |"B"|) and types.
Further more, as mentioned in Section~\ref{sec:indexed-monads}, the
dictionaries will be parameters to the indexed monad |Edis|. In a dependently
typed programming language (without the so-called ``phase distinction'' ---
separation between types and terms), this would pose no problem. In Haskell
however, the dictionaries, to index a monad, has to be a type as well.

In this section we describe how to construct a type-level dictionary, to be
used with the indexed monad in Section~\ref{sec:indexed-monads}.

\subsection{Datatype Promotion}

Haskell maintains the distinction between values, types, and kinds: values are
categorized by types, and types are categorized by kinds. The kinds are
relatively simple: |*| is the kind of all {\em lifted} types, while type
constructors have kinds such as |* -> *|, or |* -> * -> *|, etc.\footnotemark\,
Consider the datatype definitions below:
\begin{spec}
data Nat = Zero | Suc Nat {-"~~,\qquad"-} data [a] = [] | a : [a] {-"~~."-}
\end{spec}
The lefthand side is usually seen as having defined a type |Nat :: *|,
and two value constructors |Zero :: Nat| and |Suc :: Nat -> Nat|. The righthand
side is how Haskell lists are understood. The {\em kind} of |[.]| is |* -> *|,
since it takes a lifted type |a| to a lifted type |[a]|. The two value
constructors respectively have types |[] :: [a]| and |(:) :: a -> [a] ->
[a]|, for all type |a|.

\footnotetext{In Haskell, the opposite of \emph{lifted} types are \emph{unboxed}
types, which are not represented by a pointer to a heap object, and cannot be
stored in a polymorphic data type.}

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
all kind |k|, |[]| is a type of kind |[k]|. Given a type |a| of kind |k| and a
type |as| of kind |[k]|, |a : as| is again a type of kind |[k]|. Formally,
|(:) :: k -> [k] -> [k]|. For example, |Int :- (Char :- (Bool :- NIL))| is a
type having kind |[*]| --- it is a list of (lifted) types. The optional quote
denotes that the constructors are promoted. The same list can be denoted by a
syntax sugar |TList (Int, Char, Bool)|.

Tuples are also promoted. Thus we may put two types in a pair to form another
type, such as in |TPar (Int, Char)|, a type having kind |(*,*)|.

Strings in Haskell are nothing but lists of |Char|s. Regarding promotion,
however, a string can be promoted to a type having kind |Symbol|. In the expression:
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
families}, or \emph{type families} for short, are considered a cleaner solution.

For example, compare disjunction |(||||)| and its type-level
counterpart:\\
\noindent{\centering %\small
\begin{minipage}[b]{0.35\linewidth}
\begin{spec}
(||) :: Bool -> Bool -> Bool
True  ||  b  = True
a     ||  b  = b {-"~~,"-}
\end{spec}
\end{minipage}
\begin{minipage}[b]{0.55\linewidth}
\begin{spec}
type family Or (a :: Bool) (b :: Bool) :: Bool
  where  TRUE  `Or` b  = TRUE
         a     `Or` b  = b {-"~~."-}
\end{spec}
\end{minipage}
}\\
The lefthand side is a typical definition of |(||||)| by pattern matching.
On the righthand side, |Bool| is not a type, but a type lifted to a kind,
while |True| and |False| are types of kind |Bool|. The declaration says that
|Or| is a family of types, indexed by two parameters |a| and |b| of kind |Bool|.
The type with index |TRUE| and |b| is |TRUE|, and all other indices lead to |b|.
For our purpose, we can read |Or| as a function from types to types ---
observe how it resembles the term-level |(||||)|. We present two more type-level
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

As a remark, type families in Haskell come in many flavors. One can define families of |data|, as well as families of |type| synonym. They can appear
inside type classes~\cite{tfclass,tfsynonym} or at toplevel. Top-level type families can be open~\cite{tfopen} or closed~\cite{tfclosed}. The flavor we
chose is top-level, closed type synonym family, since it allows overlapping
instances, and since we need none of the extensibility provided by open type
families. Notice that the instance |TRUE `Or` b| could be subsumed under the
more general instance, |a `Or` b|. In a closed type family we may resolve the
overlapping in order, just like how cases overlapping is resolved in term-level
functions.

We are now able to define operations on type-level dictionaries. Let's begin
with dictionary lookup:
\begin{spec}
type family Get (xs :: [(Symbol, *)]) (k :: Symbol) :: * where
    Get (TPar (k, x) :- xs) k  =  x
    Get (TPar (t, x) :- xs) k  =  Get xs k {-"~~."-}
\end{spec}
The type-level function |Get| returns the entry associated with key |k| in the
dictionary |xs|. Notice, in the first case, how type-level equality can be
expressed by unifying type variables with the same name. Note also that |Get| is
a partial function on types: while |Get (TList (TPar ("A", Int))) "A"| evaluates
to |Int|, when |Get (TList (TPar ("A", Int))) "B"| appears in a type expression,
there are no applicable rules to reduce it. The expression thus stays as it is.

% For our applications it is more convenient to make |Get| total, as we would at
% the term level, by having it return a |Maybe|:
% \begin{spec}
% type family Get (xs :: [(Symbol, *)]) (k :: Symbol) :: Maybe * where
%     Get NIL                 k = Nothing
%     Get (TPar(k, x) :- xs)  k = Just x
%     Get (TPar(t, x) :- xs)  k = Get xs k {-"~~."-}
% \end{spec}
%
Some other dictionary-related functions are defined in a similar fashion
in Figure \ref{fig:dict-operations}. The function |Set| either updates an
existing entry or inserts a new entry, |Del| removes an entry matching
a given key, while |Member| checks whether a given key exists in the
dictionary.

\begin{figure}[t]
\begin{spec}
-- inserts or updates an entry
type family Set  (xs :: [(Symbol, *)]) (k :: Symbol) (x :: *) :: [(Symbol, *)] where
    Set NIL                 k x = TList (TPar (k, x))
    Set (TPar(k, y) :- xs)  k x = TPar (k, x)  :- xs
    Set (TPar(t, y) :- xs)  k x = TPar (t, y)  :- Set xs k x

-- removes an entry
type family Del  (xs :: [(Symbol, *)]) (k :: Symbol) :: [(Symbol, *)] where
    Del Nil                  k = Nil
    Del (TPar (k, y) :- xs)  k = xs
    Del (TPar (t, y) :- xs)  k = TPar (t, y) :- Del xs k

-- membership
type family Member (xs :: [(Symbol, *)]) (k :: Symbol) :: Bool where
    Member Nil                  k = False
    Member (TPar(k, x) :- xs)   k = True
    Member (TPar(t, x) :- xs)   k = Member xs k
\end{spec}
\caption{Some operations on type-level dictionaries.}
\label{fig:dict-operations}
\end{figure}
