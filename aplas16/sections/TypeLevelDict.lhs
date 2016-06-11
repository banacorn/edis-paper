%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Type-Level Dictionaries}
\label{sec:type-level-dict}

To check the bindings between keys and values, we need a \emph{dictionary-like}
 structure, and encode it as a \emph{type} somehow.

\subsection{Datatype promotion}
Normally, at the term level, we could express the datatype of dictionary with
\emph{type synonym} like this.\footnotemark

\begin{spec}
type Key = String
type Dictionary = [(Key, TypeRep)]
\end{spec}

\footnotetext{|TypeRep| supports term-level representations
 of datatypes, available in |Data.Typeable|}

To encode this in the type level, everything has to be
 \emph{promoted}\cite{promotion} one level up.
 From terms to types, and from types to kinds.

Luckily, with recently added GHC extension \emph{data kinds}, suitable
 datatype will be automatically promoted to be a kind, and its value
 constructors to be type constructors. The following type |List|

\begin{spec}
data List a = Nil | Cons a (List a)
\end{spec}

Give rise to the following kinds and type constructors:\footnote{To distinguish
 between types and promoted constructors that have
 ambiguous names, prefix promoted constructor with a single quote like
 |NIL| and |CONS|}
\footnote{All kinds have \emph{sort} BOX in Haskell\cite{sorts}}


\begin{spec}
List k :: BOX
Nil  :: List k
Cons :: k -> List k -> List k
\end{spec}

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
