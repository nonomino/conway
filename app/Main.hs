import World
import Graphics
import qualified Conway

colorCell :: Conway.CellState -> PatchColor
colorCell Conway.Alive = brown
colorCell Conway.Dead  = hunter

main :: IO ()
main = do
  game <- Conway.randomWorld 20 20
  doGraphics colorCell (evolve Conway.evolveCell) game
