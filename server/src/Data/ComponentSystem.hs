module Data.ComponentSystem (
    ComponentSystem,
    EntityId(TextId),
    newSystem,
    updateComponent,
    addComponent,
    deleteComponent,
    listComponents,
    asMarker,
    createId
    ) where


import qualified Data.Map.Strict as M
import Data.Text(Text)
import Data.Functor.Apply

data EntityId = TextId Text | IntId Int deriving (Eq, Ord) 
data ComponentSystem a = CS (M.Map EntityId a) 

newSystem :: ComponentSystem a
newSystem = CS M.empty

addComponent :: EntityId -> a -> ComponentSystem a -> ComponentSystem a
addComponent e x (CS cs) = CS $  M.insert e x cs

updateComponent :: (a -> Maybe a) -> EntityId -> ComponentSystem a -> ComponentSystem a
updateComponent f e (CS cs) = CS $  M.update f e cs

deleteComponent :: EntityId -> ComponentSystem a -> ComponentSystem a
deleteComponent e (CS cs) = CS $  M.delete e cs

instance Functor ComponentSystem where 
    fmap f (CS cs) = CS $ fmap f cs
instance Apply ComponentSystem where
    (<.>) (CS fs) (CS as) = CS $ M.mapMaybeWithKey (\k f -> f <$> M.lookup k as) fs

listComponents :: ComponentSystem a -> [(EntityId, a)]
listComponents (CS cs) = M.toList cs

asMarker :: ComponentSystem () -> ComponentSystem b -> ComponentSystem b
asMarker as bs = liftF2 (flip const) as bs

createId :: IO EntityId
createId = do 
    pure $ IntId 1