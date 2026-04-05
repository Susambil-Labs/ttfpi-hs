module Lambda where

import Data.Kind (Type)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (empty)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

type MegaParsec = Text.Megaparsec.Parsec Void Text

type Variable :: Type
newtype Variable = Variable String
    deriving stock (Show, Eq)
    deriving newtype (IsString)

type Term :: Type
data Term
    = Abstraction Variable Term -- Abstraction: (λx. M)
    | Application Term Term -- Application: (M N)
    | Var Variable -- Variable: x
    deriving stock (Show)

spaceConsumer :: MegaParsec ()
spaceConsumer =
    L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: MegaParsec a -> MegaParsec a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> MegaParsec Text
symbol = L.symbol spaceConsumer

parens :: MegaParsec a -> MegaParsec a
parens = between (symbol "(") (symbol ")")

variable :: MegaParsec Variable
variable = lexeme $ do
    first <- C.letterChar
    rest <- many C.alphaNumChar
    pure $ Variable (first : rest)

abstraction :: MegaParsec Term
abstraction = do
    _ <- lexeme (C.char '\\' <|> C.char 'λ')
    vars <- some variable
    _ <- symbol "."
    body <- term
    pure $ foldr Abstraction body vars

atom :: MegaParsec Term
atom = parens term <|> (Var <$> variable)

application :: MegaParsec Term
application = do
    atoms <- some atom
    pure $ foldl1 Application atoms

term :: MegaParsec Term
term = abstraction <|> application

parseLambda :: Text -> Either (ParseErrorBundle Text Void) Term
parseLambda = parse (spaceConsumer *> term <* eof) ""

-- >>> parseLambda "x"
-- >>> parseLambda "\\x. x"
-- >>> parseLambda "λx y. x"
-- >>> parseLambda "a b c"
-- >>> parseLambda "(\\x. x) y"
-- >>> parseLambda "λf x. f (f x)"
-- Right (Var (Variable "x"))
-- Right (Abstraction (Variable "x") (Var (Variable "x")))
-- Right (Abstraction (Variable "x") (Abstraction (Variable "y") (Var (Variable "x"))))
-- Right (Application (Application (Var (Variable "a")) (Var (Variable "b"))) (Var (Variable "c")))
-- Right (Application (Abstraction (Variable "x") (Var (Variable "x"))) (Var (Variable "y")))
-- Right (Abstraction (Variable "f") (Abstraction (Variable "x") (Application (Var (Variable "f")) (Application (Var (Variable "f")) (Var (Variable "x"))))))
