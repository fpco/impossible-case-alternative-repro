{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Module
  ( JSON(..)
  , liftNewStateT
  , runJSONParser
  , inside
  , exitWith
  , Mytype(..)
  ) where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Either
import           Data.Aeson.Types as X hiding (withObject,withArray)
import           Language.Haskell.TH


data Mytype = Mytype { mytypeValue :: Double }
  deriving (Eq,Show)


pushStack :: [()] -> [()]
pushStack xs = () : xs -- Impossible case alternative
-- pushStack xs =      xs -- No Impossible case alternative


inside :: Name -> Q Exp
inside name = [|\m -> $(onField name) (flip liftNewStateT m)|]

onField :: Name -> Q Exp
onField field =
  do VarI _ (AppT (AppT ArrowT (ConT _ty)) _) _ _ <- reify field
     [|\m ->
         do a <- fmap $(varE field) getState
            withCtx (m a)|]

-- | Get the state from the JSON.
getState :: JSON s s
getState = JSON (EitherT (StateT (\s -> return (Right (jsonStructure s),s))))

newtype JSON s a = JSON { runJSON :: EitherT () (StateT (JSONState s) Parser) a}
  deriving (Monad,Applicative,Functor)

data JSONState s =
  JSONState {jsonStack :: [()]
            ,_jsonNotifications :: [Bool]
            ,jsonStructure :: s}


runJSONParser :: JSON () a -> Either String (Either () a)
runJSONParser f =
  case parseEither
         (const (runStateT (runEitherT (runJSON f))
                           (JSONState [] [] ())))
         () of
    Right (result,_) -> return result
    Left err -> Left err

-- | Lift a state action into our monad.
liftStateT :: StateT (JSONState s) Parser a -> JSON s a
liftStateT = JSON . lift

exitWith :: JSON s a
exitWith = JSON (left ())

liftNewStateT :: t -> JSON t a -> JSON s a
liftNewStateT o m =
  JSON (EitherT (StateT (\(JSONState stack ns orig) ->
                           do (result,JSONState _ ns' _) <-
                                runStateT (runEitherT (runJSON m))
                                          (JSONState stack ns o)
                              return (result,JSONState stack ns' orig))))

withCtx :: JSON s a -> JSON s a
withCtx m =
  do s <- liftStateT get
     liftStateT (put s {jsonStack = pushStack (jsonStack s)})
     v <- m
     s' <- liftStateT get
     liftStateT (put s' { jsonStack = jsonStack s})
     return v
