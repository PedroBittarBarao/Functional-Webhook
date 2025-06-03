import Data.IORef
import qualified Data.Set as Set
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)


import WebhookEvent
import RequestHandler (app)

main :: IO ()
main = do
  putStrLn "Listening on port 5001..."
  processedRef <- newIORef Set.empty
  run 5000 (app processedRef)
