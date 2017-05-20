module Parser where

import           Control.Applicative (empty)
import           Control.Monad (void)
import           Data.Functor.Identity (Identity(..))
import qualified Data.Text as T
import           Data.Text (Text)
import           Text.Megaparsec
import           Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import qualified Lam as Lam

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf (" \t" :: String)) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parser :: Parser [Lam.Binding Identity]
parser = pBindings <* eof

pBindings :: Parser [Lam.Binding Identity]
pBindings = some $ pBinding

pName :: Parser () -> Parser Text
pName sc' = T.pack <$> (L.lexeme sc' $ some (alphaNumChar <|> char '_'))

pBinding :: Parser (Lam.Binding Identity)
pBinding = do name <- pName sc
              void $ lexeme $ string "="
              bindingContents <- pBindingContents
              pure $ Lam.Binding name bindingContents

pBindingContents :: Parser (Lam.BindingContents Identity)
pBindingContents = L.lineFold scn $ \sc' -> (Lam.BindingTypeish <$> unionContents sc')
                                             <|> (Lam.BindingExp <$> pExp sc')

unionContents :: Parser () -> Parser (Lam.Typeish Identity)
unionContents sc' = Lam.Union <$> between (L.lexeme sc' (string "{"))
                                          (L.lexeme sc' (string "}"))
                                          (sepBy (Identity <$> pName sc') (L.lexeme sc' (string ",")))

pExp :: Parser () -> Parser Lam.Exp
pExp sc' = try (pExpLit sc') <|> try (pExpVar sc') <|> try (pExpLam sc') <|> pExpParen sc' <|>  pExpRecord sc' <|> pExpSuspend sc' <|> pExpLamArgId sc' <|> pExpApp sc'

pExpParen :: Parser () -> Parser Lam.Exp
pExpParen sc' = between (L.lexeme sc' (string "("))
                        (L.lexeme sc' (string ")"))
                        (pExp sc')

pExpLam :: Parser () -> Parser Lam.Exp
pExpLam sc' = do void $ L.lexeme sc' (string "\\")
                 names <- sepBy (pName sc') (L.lexeme sc' (string ","))
                 void $ L.lexeme sc' (string "->")
                 body <- pExp sc'
                 pure $ Lam.lam names body

pExpApp :: Parser () -> Parser Lam.Exp
pExpApp sc' = do body <- pExp sc'
                 args <- some $ pArg sc'
                 pure $ Lam.app body args

pArg :: Parser () -> Parser (T.Text, Lam.Exp)
pArg sc' = do argName <- pName sc'
              void $ L.lexeme sc' (string ":")
              argBody <- pExp sc'
              pure (argName, argBody)


pExpRecord :: Parser () -> Parser Lam.Exp
pExpRecord sc' = between (L.lexeme sc' (string "{"))
                         (L.lexeme sc' (string "}"))
                         (Lam.record <$> some (pName sc'))

pExpVar :: Parser () -> Parser Lam.Exp
pExpVar sc' = Lam.var <$> pName sc'

pExpSuspend :: Parser () -> Parser Lam.Exp
pExpSuspend sc' = Lam.suspend <$> pSuspendSpec sc'

pSuspendSpec :: Parser () -> Parser (Lam.SuspendSpec Identity Lam.Exp)
pSuspendSpec sc' = do void $ L.lexeme sc' (string "'")
                      name <- pName sc'
                      pSuspendSpecDetails sc' (Lam.suspendSpec name [] [])

pSuspendSpecDetails :: Parser () -> Lam.SuspendSpec Identity Lam.Exp -> Parser (Lam.SuspendSpec Identity Lam.Exp)
pSuspendSpecDetails sc' spec = pSuspendSpecParent sc' spec <|> pSuspendSpecArg sc' spec

pSuspendSpecParent :: Parser () -> Lam.SuspendSpec Identity Lam.Exp -> Parser (Lam.SuspendSpec Identity Lam.Exp)
pSuspendSpecParent sc' spec = do void $ L.lexeme sc' (string "^")
                                 parent <- pSuspendSpec sc' <|> between (L.lexeme sc' (string "("))
                                                                        (L.lexeme sc' (string ")"))
                                                                        (pSuspendSpec sc')
                                 pSuspendSpecDetails sc' $ Lam.suspendSpecWithParent spec parent

pSuspendSpecArg :: Parser () -> Lam.SuspendSpec Identity Lam.Exp -> Parser (Lam.SuspendSpec Identity Lam.Exp)
pSuspendSpecArg sc' spec = do arg <- pArg sc'
                              pSuspendSpecDetails sc' $ Lam.suspendSpecWithArg spec arg



pExpLamArgId :: Parser () -> Parser Lam.Exp
pExpLamArgId sc' = do void $ L.lexeme sc' (string "*")
                      Lam.lamArgId <$> pName sc'

pExpLit :: Parser () -> Parser Lam.Exp
pExpLit sc' = Lam.lit <$> (pExpString sc' <|> pExpInteger sc')

pExpString :: Parser () -> Parser Lam.Literal
pExpString sc' = Lam.Text . T.pack <$> between (L.lexeme sc' (string "\""))
                                               (L.lexeme sc' (string "\""))
                                               (many $ noneOf ("\"" :: String))


pExpInteger :: Parser () -> Parser Lam.Literal
pExpInteger sc' = (Lam.Number . read) <$> L.lexeme sc' (some digitChar)


