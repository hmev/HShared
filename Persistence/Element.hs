module Persistence.Element where
import Text.JSON
import Element.Element

instance JSON Element where
    readJSON (JSObject js)
        = case fromJSObject js of 
            [("Name", sname), ("Episode", seps)] 
                      -> do name <- readJSON sname
                            eps <- readJSON seps
                            return $ Element name eps
            otherwise -> Error "Wrong JSON."
    readJSON _ = Error "Wrong JSON."
    showJSON (Element name eps) 
        = makeObj [("Name",   showJSON name)
                  ,("Episode",showJSON eps)
                  ]

instance JSON ElementHistory where
    readJSON (JSObject js)
        = case fromJSObject js of 
            [("Modifed", smod)] 
                      -> do mod <- readJSON smod
                            return $ ElementHistory mod
            otherwise -> Error "Wrong JSON."
    readJSON _ = Error "Wrong JSON."  
    showJSON (ElementHistory mod)
        = makeObj [("Modified", showJSON mod)]