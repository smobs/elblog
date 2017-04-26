module Data.ComponentSystem (
    ComponentSystem,
    EntityId,
    newSystem,
    updateComponent,
    addComponent,
    deleteComponent,
    liftSystem,
    listComponents
    ) where


import qualified Data.Map.Strict as M
import Data.Text(Text)

type EntityId = Text
data ComponentSystem a = CS (M.Map EntityId a) 

newSystem :: ComponentSystem a
newSystem = CS M.empty

addComponent :: EntityId -> a -> ComponentSystem a -> ComponentSystem a
addComponent e x (CS cs) = CS $  M.insert e x cs

updateComponent :: (a -> Maybe a) -> EntityId -> ComponentSystem a -> ComponentSystem a
updateComponent f e (CS cs) = CS $  M.update f e cs

deleteComponent :: EntityId -> ComponentSystem a -> ComponentSystem a
deleteComponent e (CS cs) = CS $  M.delete e cs


liftSystem :: (a -> b -> c) -> ComponentSystem a -> ComponentSystem b -> ComponentSystem c
liftSystem f (CS as) (CS bs) = CS $ M.mapMaybeWithKey (\k b -> f <$> (M.lookup k as) <*> (pure b)) bs

listComponents :: ComponentSystem a -> [(EntityId, a)]
listComponents (CS cs) = M.toList cs