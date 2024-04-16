import Data.Array

--definições de tipos para facilitar a leitura e manutenção do código.
type Tabuleiro = Array (Int, Int) Char  --o tabuleiro é um array 2D de caracteres.
type Coordenada = (Int, Int)  --uma coordenada no tabuleiro é representada por um par de inteiros.

--inicializa o tabuleiro com água (~) em todas as posições.
inicializaTabuleiro :: Tabuleiro
inicializaTabuleiro = array ((1, 1), (10, 10)) [((i, j), '~') | i <- [1..10], j <- [1..10]]

--lista de coordenadas que representam a posição fixa de um navio no tabuleiro.
coordenadasNavio :: [Coordenada]
coordenadasNavio = [(2, 2), (2, 3), (2, 4)]

--função para atualizar o tabuleiro com o resultado de um tiro: acerto ('X') ou água ('o').
atira :: Coordenada -> Tabuleiro -> Tabuleiro
atira coord tabuleiro = tabuleiro // [(coord, marca)]
  where marca = if coord `elem` coordenadasNavio then 'X' else 'o'

--função para imprimir o tabuleiro no terminal, linha por linha.
imprimeTabuleiro :: Tabuleiro -> IO ()
imprimeTabuleiro tabuleiro = mapM_ print [ [tabuleiro ! (i,j) | j <- [1..10]] | i <- [1..10]]

--função principal que inicia o jogo.
main :: IO ()
main = do
  let tabuleiro = inicializaTabuleiro
  loopJogo tabuleiro

--função auxiliar para converter a string de entrada do usuário em uma tupla de Int (coordenadas).
analisaEntrada :: String -> Coordenada
analisaEntrada entrada = (x, y)
  where [xStr, yStr] = words entrada  --separa a string de entrada em duas, usando espaço como delimitador.
        x = read xStr :: Int  --converte a primeira parte da entrada para Int.
        y = read yStr :: Int  --converte a segunda parte da entrada para Int.

--verifica se todas as partes do navio foram acertadas, indicando se o navio foi afundado.
navioAfundado :: Tabuleiro -> [Coordenada] -> Bool
navioAfundado tabuleiro navio = all (\coord -> tabuleiro ! coord == 'X') navio  -- Verifica se todas as coordenadas do navio contêm 'X'.

-- loop principal do jogo. Gerencia a lógica de turnos e verifica o estado do jogo após cada tiro.
loopJogo :: Tabuleiro -> IO ()
loopJogo tabuleiro = do
  imprimeTabuleiro tabuleiro  -- imprime o estado atual do tabuleiro.
  putStrLn "Digite as coordenadas do tiro (Formato: x y):"  --solicita ao usuário que digite as coordenadas do tiro.
  entrada <- getLine  --recebe a entrada do usuário.
  let coords = analisaEntrada entrada  --converte a entrada do usuário para coordenadas.
  let novoTabuleiro = atira coords tabuleiro  --atualiza o tabuleiro com o tiro.
  
  --verifica se o tiro foi um acerto ou erro, e imprime uma mensagem correspondente.
  if novoTabuleiro ! coords == 'X'
    then putStrLn "Você acertou um navio!"
    else putStrLn "Água!"
  
  --verifica se todos os navios foram afundados. Se sim, termina o jogo; senão, continua.
  if navioAfundado novoTabuleiro coordenadasNavio
    then putStrLn "Todos os navios foram afundados! Fim de jogo."
    else loopJogo novoTabuleiro  --continua o loop do jogo se nem todos os navios foram afundados.
