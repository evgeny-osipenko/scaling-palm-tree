module Data.Validation
    ( Validation (.., Errors, Valid)
    )
where

newtype Validation e a = Validation
    { runValidation ::
        Either [e] a
    }
  deriving newtype (Functor)

pattern Errors :: forall e a. [e] -> Validation e a
pattern Errors e = Validation (Left e)

pattern Valid :: forall e a. a -> Validation e a
pattern Valid x = Validation (Right x)

{-# COMPLETE Errors, Valid #-}

instance Applicative (Validation e) where
    pure = Valid
    Valid f <*> Valid x = Valid (f x)
    Errors ea <*> Valid _ = Errors ea
    Valid _ <*> Errors eb = Errors eb
    Errors ea <*> Errors eb = Errors (ea <> eb)
