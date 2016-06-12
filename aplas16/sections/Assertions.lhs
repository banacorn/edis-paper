%include lhs2TeX.fmt
%include polycode.fmt
%include Formatting.fmt

\section{Assertions}
\label{sec:assertions}

Users may need to make assertions about the status of some key-type bindings in
 a Redis program. Specifically, when declaring or renouncing the existence of
 a key and the type of its associating value. We provide these functions,
 which do nothing but fiddle with types.

\begin{spec}
declare :: (KnownSymbol s, Member xs s ~ False)
        => Proxy s
        -> Proxy x  -- type of value
        -> Edis xs (Set xs s x) ()
declare s x = Edis $ return ()

renounce :: (KnownSymbol s, Member xs s ~ True)
        => Proxy s -> Edis xs (Del xs s) ()
renounce s = Edis $ return ()

-- to be used at the beginning of a program
start :: Edis ’[] ’[] ()
start = Edis $ return ()
\end{spec}

\subsection{A complete example}

The following program increases the value of |"A"| as an integer, push the result
 of the increment to list |"L"|, and then pops it out.

\begin{spec}
main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runRedis conn $ unEdis $ start
        `bind` \_ ->  declare
                        (Proxy :: Proxy "A")
                        (Proxy :: Proxy Integer)
        `bind` \_ ->  incr     (Proxy :: Proxy "A")
        `bind` \n ->  case n of
            Left  err -> lpush (Proxy :: Proxy "L") 0
            Right n   -> lpush (Proxy :: Proxy "L") n
        `bind` \_ ->  lpop     (Proxy :: Proxy "L")
    print result
\end{spec}

The syntax is pretty heavy, like the old days when there's no
 \emph{do-notation}\cite{history}. But if we don't need any variable bindings
 between operations, we could compose these commands with a sequencing operator
 |(>>>)|.

\begin{spec}
(>>>) :: IMonad m => m p q a -> m q r b -> m p r b
\end{spec}
\begin{spec}
program = start
    >>> declare
            (Proxy :: Proxy "A")
            (Proxy :: Proxy Integer)
    >>> incr    (Proxy :: Proxy "A")
    >>> lpush   (Proxy :: Proxy "L") 0
    >>> lpop    (Proxy :: Proxy "L")
\end{spec}
