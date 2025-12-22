module FreeVariable where

import Data.List
import Data.String (IsString (..))

newtype Variable = Variable String
    deriving stock (Show, Eq)
    deriving newtype (IsString)

data Term
    = Abstraction Variable Term -- Abstraction: (λx. M)
    | Application Term Term -- Application: (M N)
    | Var Variable -- Variable: x
    deriving stock (Show)

fv :: Term -> [Variable]
fv (Var x) = [x]
fv (Application m n) = fv m `union` fv n
fv (Abstraction x m) = fv m \\ [x]

x :: Variable
x = "x"

y :: Variable
y = Variable "y"
test = fv (Abstraction x (Application (Var x) (Var y)))
test2 = fv (Abstraction x ((Var y)))
