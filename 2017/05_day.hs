import Control.Monad.ST ( ST, runST )
import Data.STRef ( newSTRef, writeSTRef )
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

part1 :: Int
part1 = runST $ do
    xs <- V.thaw $ V.fromList ([1..10] :: [Int])
    res <- newSTRef 0

    jmp xs res 0

    return 1
    where
        -- TODO do this using lambda?
        jmp vec r n = do
            if n == 10
                then do VM.write vec 0 1
                        jmp vec r (n + 1)
                else writeSTRef r n