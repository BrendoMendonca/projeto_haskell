-- Importando a função 'transpose' de 'Data.List' para manipular colunas e diagonais do tabuleiro
import Data.List (transpose)

-- Definindo o tipo 'Tabuleiro' como uma lista de listas de caracteres
type Tabuleiro = [[Char]]

-- Função que gera um tabuleiro inicial vazio
tabuleiroInicial :: Tabuleiro
tabuleiroInicial = replicate 3 [' ', ' ', ' ']

-- Função para imprimir o tabuleiro no console, com coordenadas
imprimirTabuleiro :: Tabuleiro -> IO ()
imprimirTabuleiro tabuleiro = do
  putStrLn "   0   1   2" -- Imprime o cabeçalho das colunas
  mapM_ putStrLn $ zipWith (\linha linhaTab -> show linha ++ " " ++ unwords [show celula | celula <- linhaTab]) [0..] tabuleiro
  -- ^ Para cada linha do tabuleiro, imprime o número da linha seguido pelas células da linha

-- Função que atualiza o tabuleiro com a jogada do jogador
fazerJogada :: Tabuleiro -> (Int, Int) -> Char -> Tabuleiro
fazerJogada tabuleiro (linha, coluna) jogador =
  take linha tabuleiro ++
  [take coluna (tabuleiro !! linha) ++ [jogador] ++ drop (coluna + 1) (tabuleiro !! linha)] ++
  drop (linha + 1) tabuleiro
  -- ^ Substitui a célula especificada pela jogada do jogador

-- Função para verificar se há uma condição de vitória no tabuleiro
verificarVitoria :: Tabuleiro -> Bool
verificarVitoria tab = any linhaCompleta (linhas ++ colunas ++ diagonais)
  where
    linhas = tab
    colunas = transpose tab
    diagonais = [diagonal tab, diagonal (map reverse tab)]
    -- ^ Calcula as diagonais do tabuleiro
    diagonal t = [t !! i !! i | i <- [0..2]]
    linhaCompleta linha = all (== 'X') linha || all (== 'O') linha
    -- ^ Verifica se uma linha, coluna ou diagonal está completa

-- Função que verifica se o tabuleiro está cheio (sem espaços vazios)
tabuleiroCheio :: Tabuleiro -> Bool
tabuleiroCheio tabuleiro = not (any (elem ' ') tabuleiro)

-- Função principal que gerencia o jogo, alternando os jogadores e verificando o estado do jogo
jogarJogo :: Tabuleiro -> Char -> IO ()
jogarJogo tabuleiro jogador = do
  imprimirTabuleiro tabuleiro -- Mostra o estado atual do tabuleiro
  putStrLn $ "Vez do jogador " ++ [jogador] ++ ". Insira linha e coluna:"
  linha <- getLine
  coluna <- getLine
  let novoTabuleiro = fazerJogada tabuleiro (read linha, read coluna) jogador
  -- ^ Atualiza o tabuleiro com a jogada do jogador
  imprimirTabuleiro novoTabuleiro -- Opcional: Mostra o tabuleiro após a jogada
  if verificarVitoria novoTabuleiro
    then putStrLn $ "Jogador " ++ [jogador] ++ " venceu!"
    else if tabuleiroCheio novoTabuleiro
      then putStrLn "O jogo terminou em empate."
      else jogarJogo novoTabuleiro (if jogador == 'X' then 'O' else 'X')
      -- ^ Se não houve vitória ou empate, passa a vez para o outro jogador

-- Função 'principal' que inicia o jogo com o jogador 'X'
main :: IO ()
main = jogarJogo tabuleiroInicial 'X'