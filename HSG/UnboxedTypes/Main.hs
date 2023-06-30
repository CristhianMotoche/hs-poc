{-# LANGUAGE MagicHash #-}

module Main where

{- Starting with the primitives -}
{-

  We'll be using *GHC*!

  What happens when we do the following sum in Haskell?

  >>>  1 + 2 :: Int

  The (+) operator actually looks like this for Int:

  >>> instance Num Int where
  >>>     I# x + I# y = I# (x +# y)

  `Int` actually looks like an algebraic data type:

  >>> data Int = I# Int#

  And `+#` looks like this:

  infixl 6 +#
  (+#) :: Int# -> Int# -> Int#
  (+#) = let x = x in x

  What is this?
    It is a bottom type (like `undefined`)

    It's a primary operation -> primops

    When GHC reaches a call to one of these primops,
    it automatically replaces it with the real implementation for you,
    which will be some assembly code, LLVM code, or something similar.

    These operations and types end in # which is called the 'Magic Hash':

    >>>:set -XMagicHash

    Convention to separate between boxed and unboxed types.
-}

{-
   I# is a normal constructor

   Int# looks like: data Int# and it's

   a normal long int from C (32-bit or 64-bit, depending on architecture).

   Try in GHCi:

   >>> :kind Int
   >>> :kind Int#

   Quoting GHC docs:

    > Most types in GHC are boxed, which means that values of that type are
    > represented by a pointer to a heap object. The representation of a Haskell
    > Int, for example, is a two-word heap object. An unboxed type, however,
    > is represented by the value itself, no pointers or heap allocation
    > are involved.

    -------
    --STACK
    --calls
    -------
    --HEAP
    --data
    -------
-}

{-

   In the following code:

     main = do
         let x = 1 + 2
             y = 3 + 4
         print x
         print y

    which expression is executed first: 1 + 2 or 3 + 4?

    GHC is fully within its rights to rearrange evaluation of those expressions
    So, there could be cases in which 3 + 4 came before 1 + 2 and viceversa.

    Then, could there be cases where `print y` occurs before `print x`?
    They're not tied after all.

    First things first, let's remove some sugar:

      main :: IO ()
      main = print 3 >> print 7

    So, what is IO:

      newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

    Things to note here:

      * State# has a parameter (we'll see it later)
      * (# .. #) is an unboxed type -> no extra allocation and no thunks
      * IO takes a real world state, and gives  back a real world state and some value
      * By creating a dependency on the result of a previous function,
        we are able to ensure evaluation order, yet still remain purely functional.

    Looking at the Monad IO instance:

      instance  Monad IO  where
            (>>) = thenIO

      thenIO :: IO a -> IO b -> IO b
      thenIO (IO m) k =
          IO $ \ s ->
              case m s of
                (# new_s, _ #) -> unIO k new_s

      unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
      unIO (IO a) = a

    So, `print 3 >> print 7` will look like this:

      main = IO $ \s0 ->
        case unIO (print 3) s0 of
          (# s1, res1 #) -> unIO (print 7) s1

    Things to note here:
      * we ignore the result of print 3 (the `res1` value)
      * we still depend on the new state token `s1` to evaluate `print 7`
      * This forces the order of evaluation to first evaluate `print 3`
        and then evaluate `print 7`

    Exercise:
    1) Implement `getMaskingState :: IO Int` using `getMaskingState#`
    -  https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Exts.html#v:getMaskingState-35-
    Sort of solution:
    - https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-IO.html#v:getMaskingState
-}

{-
   Comparing ST s a and IO:

     newtype IO   a = IO (State# RealWorld -> (# State# RealWorld, a #))
     newtype ST s a = ST (State# s         -> (# State# s,         a #))

    ST RealWorld = IO -> Isomorphic

    Exercise: write a pair of functions to convert between IO a and ST RealWorld a.
      - stToIO
      - ioToST

-}


{-
Sources:
[1] Haskell primitives: https://www.fpcomplete.com/haskell/tutorial/primitive-haskell/

-}
