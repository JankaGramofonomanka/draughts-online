import qualified Testing.TestJSON as JSON
import qualified Testing.TestGame as Game


main :: IO ()
main = JSON.main
    >> Game.main
