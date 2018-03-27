module Element.EpisodeManager where
import Prerequisite
import Element.Episode

data EpisodeManager = EpisodeManager {
    current :: Episode
}

type MEpisodeManager = StateVar EpisodeManager

new :: IO MEpisodeManager
new = do newStateVar (EpisodeManager 0) 