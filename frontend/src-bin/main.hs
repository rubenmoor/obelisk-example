import           Frontend               (frontend)
import           Obelisk.Frontend       (runFrontend)
import           Obelisk.Route.Frontend (checkEncoder)
import           Reflex.Dom             (run)
import           Common.Route           (fullRouteEncoder)

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder fullRouteEncoder
  run $ runFrontend validFullEncoder frontend
