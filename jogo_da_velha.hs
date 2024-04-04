import Data.List (transpose)

type Tabuleiro = [[Char]]

tabuleiroInicial :: Tabuleiro
tabuleiroInicial = replicate 3 [' ', ' ', ' ']

imprimirTabuleiro :: Tabuleiro -> IO ()
imprimirTabuleiro tabuleiro = do
  putStrLn "   0   1   2"
  mapM_ putStrLn $ zipWith (\linha linhaTab -> show linha ++ " " ++ unwords [show celula | celula <- linhaTab]) [0..] tabuleiro

fazerJogada :: Tabuleiro -> (Int, Int) -> Char -> Tabuleiro
fazerJogada tabuleiro (linha, coluna) jogador =
  take linha tabuleiro ++
  [take coluna (tabuleiro !! linha) ++ [jogador] ++ drop (coluna + 1) (tabuleiro !! linha)] ++
  drop (linha + 1) tabuleiro

verificarVitoria :: Tabuleiro -> Bool
verificarVitoria tab = any linhaCompleta (linhas ++ colunas ++ diagonais)
  where
    linhas = tab
    colunas = transpose tab
    diagonais = [diagonal tab, diagonal (map reverse tab)]
    diagonal t = [t !! i !! i | i <- [0..2]]
    linhaCompleta linha = all (== 'X') linha || all (== 'O') linha

tabuleiroCheio :: Tabuleiro -> Bool
tabuleiroCheio tabuleiro = not (any (elem ' ') tabuleiro)

jogarJogo :: Tabuleiro -> Char -> IO ()
jogarJogo tabuleiro jogador = do
  imprimirTabuleiro tabuleiro
  putStrLn $ "Vez do jogador " ++ [jogador] ++ ". Insira linha e coluna:"
  linha <- getLine
  coluna <- getLine
  let novoTabuleiro = fazerJogada tabuleiro (read linha, read coluna) jogador
  imprimirTabuleiro novoTabuleiro -- Opcional: Mostra o tabuleiro após a jogada
  if verificarVitoria novoTabuleiro
    then putStrLn $ "Jogador " ++ [jogador] ++ " venceu!"
    else if tabuleiroCheio novoTabuleiro
      then putStrLn "O jogo terminou em empate."
      else jogarJogo novoTabuleiro (if jogador == 'X' then 'O' else 'X')

principal :: IO ()
principal = jogarJogo tabuleiroInicial 'X'
