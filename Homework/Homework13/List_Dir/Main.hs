import System.Directory (listDirectory)
import Data.List (isInfixOf, sort)
--import qualified Data.Map
--import Data.Map as DM
import Data.Map (Map, fromList)

-- >>> listDirectory "."
-- ["bin","chrome_100_percent.pak","chrome_200_percent.pak","Code.exe","Code.VisualElementsManifest.xml","d3dcompiler_47.dll","ffmpeg.dll","icudtl.dat","libEGL.dll","libGLESv2.dll","LICENSES.chromium.html","locales","policies","resources","resources.pak","snapshot_blob.bin","tools","unins000.dat","unins000.exe","unins000.msg","v8_context_snapshot.bin","vk_swiftshader.dll","vk_swiftshader_icd.json","vulkan-1.dll"]
--

find :: String -> IO (Map Int String)
find st = do
    entries <- listDirectory "."
    let found    = sort $ filter (isInfixOf st) entries 
    let foundMap = fromList $ zip ([1 ..] :: [Int]) found
    return foundMap

-- >>> find' "."
-- ["chrome_100_percent.pak","chrome_200_percent.pak","Code.exe","Code.VisualElementsManifest.xml","d3dcompiler_47.dll","ffmpeg.dll","icudtl.dat","libEGL.dll","libGLESv2.dll","LICENSES.chromium.html","resources.pak","snapshot_blob.bin","unins000.dat","unins000.exe","unins000.msg","v8_context_snapshot.bin","vk_swiftshader.dll","vk_swiftshader_icd.json","vulkan-1.dll"]
--
main :: IO (Map Int String)
main = do
    putStrLn "Provide a searh term:"
    st <- getLine
    r  <- find st
    return r










