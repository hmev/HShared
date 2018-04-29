module Element.EpisodeManager where
import Utility
import Element.Episode

data EpisodeManager = EpisodeManager {
    current :: Episode
}

type MEpisodeManager = StateVar EpisodeManager

new :: IO MEpisodeManager
new = do newStateVar (EpisodeManager 0) 