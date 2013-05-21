import Data.Yaml.YamlLight
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import Control.Applicative
import System.Environment
import Data.Maybe
import Control.Arrow

type Valsi = (String, [(String, String)])

readValsi :: FilePath -> IO (Maybe [Valsi])
readValsi fp = yamlToValsis <$> parseYamlFile fp

main :: IO ()
main = getArgs >>= readValsi . head >>= maybe (putStrLn "bad") (mapM_ printValsi)

printValsi :: Valsi -> IO ()
printValsi = putStr . showValsi

showValsi :: Valsi -> String
showValsi (ponjo, lojbo) = ponjo ++ ":" ++
	concatMap (("\n  " ++) . \(x, y) -> x ++ ": " ++ y) lojbo ++ "\n"

yamlToValsis :: YamlLight -> Maybe [Valsi]
yamlToValsis yaml =
	mapM (\(x, y) -> (,) <$> unStr' x <*> unDict y) =<< unListPair yaml

unListPair :: YamlLight -> Maybe [(YamlLight, YamlLight)]
unListPair = fmap Map.toList . unMap

unStr' :: YamlLight -> Maybe String
-- unStr' = fmap BSC.unpack . unStr
-- unStr' = fmap decodeUTF8 . unStr
unStr' = fmap BSU.toString . unStr

decodeUTF8 :: BSC.ByteString -> String
decodeUTF8 bStr = reverse $
	fst $ fromJust $ last $ takeWhile isJust $ flip iterate (Just ("", bStr)) $
	((\(str, bs) -> ((: str) *** flip BSC.drop bs) <$> BSU.decode bs) =<<)

unDict :: YamlLight -> Maybe [(String, String)]
unDict dict = mapM (\(x, y) -> (,) <$> unStr' x <*> unStr' y) =<< unListPair dict
