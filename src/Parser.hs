module Parser where

import           Control.Applicative (empty)
import           Control.Monad (void)
import           Data.Char (isSpace)
import           Data.Functor.Identity (Identity(..))
import qualified Data.Text as T
import           Data.Text (Text)
import           Text.Megaparsec
import           Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import qualified Lam as Lam

collapseCode :: String -> String
collapseCode = unlines . collapseLines . lines

collapseLines :: [String] -> [String] -- this is a grotesque hack. TODO: don't do this.
collapseLines [] = []
collapseLines (l:ls) = (l ++ (concat . map (' ':) . map (dropWhile isSpace) $ joiners)):collapseLines rest
  where (joiners, rest) = span isJoinable ls
        isJoinable [] = True
        isJoinable (c:_) = isSpace c

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
pBindings = many pBinding

pName :: Parser Text
pName = T.pack <$> (lexeme $ some (alphaNumChar <|> char '_'))

pBinding :: Parser (Lam.Binding Identity)
pBinding = do name <- pName
              void $ lexeme $ string "="
              bindingContents <- pBindingContents
              string "\n"
              pure $ Lam.Binding name bindingContents

pBindingContents :: Parser (Lam.BindingContents Identity)
pBindingContents = (Lam.BindingTypeish <$> unionContents)
                       <|> (Lam.BindingExp <$> pExp)

unionContents :: Parser (Lam.Typeish Identity)
unionContents = Lam.Union <$> between (lexeme (string "Union {"))
                                      (lexeme (string "}"))
                                      (many (Identity <$> pName))

pNonAppExp :: Parser Lam.Exp
pNonAppExp = pExpLit <|> pExpVar <|> pExpLam <|> pExpParen <|>  pExpRecord <|> pExpSuspend <|> pExpLamArgId <|> pExpList

buildAppOrExp :: Lam.Exp -> [(T.Text, Lam.Exp)] -> Lam.Exp
buildAppOrExp base [] = base
buildAppOrExp base args = Lam.app base args

pExp :: Parser Lam.Exp
pExp = do base <- pNonAppExp
          args <- many (pArg pNonAppExp)
          pure $ buildAppOrExp base args

pArg :: Parser Lam.Exp -> Parser (T.Text, Lam.Exp)
pArg expParser = do argName <- pName
                    void $ lexeme (string ":")
                    argBody <- expParser
                    pure (argName, argBody)

pExpParen :: Parser Lam.Exp
pExpParen = between (lexeme (string "("))
                    (lexeme (string ")"))
                    (pExp)

pExpLam :: Parser Lam.Exp
pExpLam = do void $ lexeme (string "\\(")
             names <- sepBy (pName) (lexeme (string ","))
             void $ lexeme (string "->")
             body <- pExp
             void $ lexeme (string ")")
             pure $ Lam.lam names body


pExpRecord :: Parser Lam.Exp
pExpRecord = Lam.record <$> between (lexeme (string "{"))
                                    (lexeme (string "}"))
                                    (some pName)

pExpVar :: Parser Lam.Exp
pExpVar = Lam.var <$> pName

pExpSuspend :: Parser Lam.Exp
pExpSuspend = between (lexeme (string "'("))
                      (lexeme (string ")"))
                      (Lam.suspend <$> pSuspendSpec)

pSuspendSpec :: Parser (Lam.SuspendSpec Identity Lam.Exp)
pSuspendSpec = do name <- pName
                  argsParents <- many $ pSuspendSpecDetails
                  let args = concat $ fmap fst argsParents
                  let parents = concat $ fmap snd argsParents
                  pure $ Lam.suspendSpec name args parents

pSuspendSpecDetails :: Parser ([(Text, Lam.Exp)], [Lam.SuspendSpec Identity Lam.Exp])
pSuspendSpecDetails = ((([],) . pure) <$> pSuspendSpecParent)
                             <|> (((,[]) . pure) <$> pArg pExp)

pSuspendSpecParent :: Parser (Lam.SuspendSpec Identity Lam.Exp)
pSuspendSpecParent = between (lexeme (string "^("))
                             (lexeme (string ")"))
                             (pSuspendSpec)


pExpLamArgId :: Parser Lam.Exp
pExpLamArgId = do void $ lexeme (string "*")
                  Lam.lamArgId <$> pName

pExpList :: Parser Lam.Exp
pExpList = do elems <- between (lexeme (string "["))
                               (lexeme (string "]"))
                               (sepBy (pExp) (lexeme $ string ","))
              pure $ foldr (\newElem list -> 
                               Lam.app (Lam.var "listAdd")
                                       [ ("listAdd_list", list)
                                       , ("listAdd_elem", newElem)
                                       ]
                           )
                           (Lam.var "listEmpty")
                           elems

pExpLit :: Parser Lam.Exp
pExpLit = Lam.lit <$> (pExpString <|> pExpInteger)

pExpString :: Parser Lam.Literal
pExpString = Lam.Text . T.pack <$> between (lexeme (string "\""))
                                           (lexeme (string "\""))
                                           (many $ noneOf ("\"" :: String))


pExpInteger :: Parser Lam.Literal
pExpInteger = (Lam.Number . read) <$> lexeme (some digitChar)


