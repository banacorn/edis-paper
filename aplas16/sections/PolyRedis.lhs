%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Making Redis Polymorphic}
\label{sec:polymorphic-redis}

Redis supports many different datatypes, these datatypes as can be viewed as
 \emph{containers} of strings. For example, lists (of strings),
 sets (of strings), and strings themselves.

\subsection{Denoting Containers}
Most Redis commands only work with a certain type of these containers. To
 annotate what container a key is associated with, we introduce these types for
 the universe of containers.

\begin{spec}
data Strings
data Lists
data Sets
...
\end{spec}

\text{SET} stores a string, regardless the datatype the key was
 associated with. Now we could implement \text{SET} like this:

\begin{spec}
set :: KnownSymbol s
    => Proxy s
    -> ByteString       -- data to store
    -> Popcorn xs (Set xs s Strings) (Either Reply Status)
set key val = Popcorn $ Hedis.set (encodeKey key) val
\end{spec}

After \text{SET}, the key will be associated with
 |Strings| in the dictionary, indicating that it's a string.

\subsection{Automatic Data Serialization}

But in the real world, raw binary strings are hardly useful, people would
 usually serialize their data into strings before storing them, and deserialize
 them back when in need.

Instead of letting users writing these boilerplates, we can do these
 serializations/deserializations for them. With the help from
 \text{cereal}, a binary serialization library.
 \text{cereal} comes with these two functions:

\begin{spec}
encode :: Serialize a
       => a -> ByteString Source

decode :: Serialize a
       => ByteString -> Either String a
\end{spec}

Which would do all the works for us, as long as the datatype it's handling is
 an instance of class |Serialize|.\footnotemark

\footnotetext{The methods of |Serialize| will have default
 generic implementations for all datatypes with some language extensions
 enabled, no sweat!}

\subsection{Extending container types}
We rename container types and extend it with an extra type argument,
 to indicate what kind of encoded value it's holding.

\begin{spec}
data StringOf x
data ListOf x
data SetOf x
...
\end{spec}

|set| reimplemented with extended container types:

\begin{spec}
set :: (KnownSymbol s, Serialize x)
    => Proxy s
    -> x       -- can be anything, as long as it's serializable
    -> Popcorn xs (Set xs s (StringOf x)) (Either Reply Status)
set key val = Popcorn $ Hedis.set (encodeKey key) (encode val)
\end{spec}

For example, if we execute |set (Proxy :: Proxy "A") True|,
 a new entry \text{'}|("A", StringOf Bool)| will
 be inserted to the dictionary.

\subsection{Handling \text{INCR}}

Commands such as \text{INCR} and \text{INCRBYFLOAT}, are
 not only container-specific, they also have some requirements on what types of
 value they could operate with.

We could handle this by mapping Redis's strings of integers and floats to
 Haskell's \text{Integer} and \text{Double}.

\begin{spec}
incr :: (KnownSymbol s
      , Get xs s ~ Just (StringOf Integer))
     => Proxy s
     -> Popcorn xs xs (Either Reply Integer)
incr key = Popcorn $ Hedis.incr (encodeKey key)

incrbyfloat :: (KnownSymbol s
     , Get xs s ~ Just (StringOf Double))
    => Proxy s
    -> Double
    -> Popcorn xs xs (Either Reply Double)
incrbyfloat key n =
    Popcorn $ Hedis.incrbyfloat (encodeKey key) n
\end{spec}
