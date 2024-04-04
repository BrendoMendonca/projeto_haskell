import Data.Array


-- Definições básicas
type Board = Array (Int, Int) Char
type Coord = (Int, Int)

-- Inicializa o tabuleiro com água
initBoard :: Board
initBoard = array ((1, 1), (10, 10)) [((i, j), '~') | i <- [1..10], j <- [1..10]]

-- Posição fixa do navio
shipCoords :: [Coord]
shipCoords = [(2, 2), (2, 3), (2, 4)]

-- Atualiza o tabuleiro com um tiro
shoot :: Coord -> Board -> Board
shoot coord board = board // [(coord, mark)]
  where mark = if coord `elem` shipCoords then 'X' else 'o'

-- Imprime o tabuleiro
printBoard :: Board -> IO ()
printBoard board = mapM_ print [ [board ! (i,j) | j <- [1..10]] | i <- [1..10]]

-- Lógica principal
main :: IO ()
main = do
  let board = initBoard
  gameLoop board


-- Função auxiliar para converter a entrada do usuário em uma tupla de Int
parseInput :: String -> (Int, Int)
parseInput input = (x, y)
  where [xStr, yStr] = words input
        x = read xStr :: Int
        y = read yStr :: Int

-- Verifica se o navio foi completamente afundado
isShipSunk :: Board -> [Coord] -> Bool
isShipSunk board ship = all (\coord -> board ! coord == 'X') ship




gameLoop :: Board -> IO ()
gameLoop board = do
  printBoard board
  putStrLn "Digite as coordenadas do tiro (Formato: x y):"
  input <- getLine
  let coords = parseInput input  -- Assume que você tem uma função parseInput que converte a string de entrada para uma tupla (Int, Int)
  let newBoard = shoot coords board
  if newBoard ! coords == 'X'
    then putStrLn "Você acertou um navio!"
    else putStrLn "Água!"
  
  -- Verifica se o navio (ou os navios) foi completamente afundado
  if isShipSunk newBoard shipCoords
    then putStrLn "Todos os navios foram afundados! Fim de jogo."
    else gameLoop newBoard
