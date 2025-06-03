import Network.Wai.Handler.Warp (run)

import WebhookEvent
import RequestHandler

main :: IO ()
main = do
  putStrLn "Listening on port 5001..."
  run 5000 app

