module NOM.Print.ProgressBar (
  printProgressBar,

  -- * Only exported for tests
  clampToByte,
  progressByteToBrailleChar,
) where

import Language.Haskell.TH.Lib
import NOM.Print.ProgressBar.Char (progressByteToBrailleChar, word5ToWord8)
import Numeric.Extra (intToDouble)
import Relude

{- | 'printProgressBar len progress' prints a progress bar of length 'len' which is filled by 'progress' ∈ [0,1]
>>> printProgressBar 3 0.477
"\10495\10247\10240"
-}
printProgressBar :: Int -> Double -> Text
printProgressBar len progress = toText $ lookupProgressChar . progressWord5AtPosition <$> [0, 1 .. len - 1]
 where
  -- progressStretchedToWholeBar ∈ [0,len]
  progressStretchedToWholeBar :: Double
  progressStretchedToWholeBar = progress * intToDouble len
  -- Takes the position in the bar and returns the progress of the bar within that position between 0 and 31.
  progressWord5AtPosition :: Int -> Word8
  progressWord5AtPosition position = clampToByte $ 0b10_0000 * (progressStretchedToWholeBar - intToDouble position)

lookupProgressChar :: Word8 -> Char
lookupProgressChar =
  $( lamCaseE
       $ [0 .. 31 :: Word8]
       <&> \n -> match (litP (integerL (toInteger n))) (normalB (litE (charL $ progressByteToBrailleChar (word5ToWord8 n)))) []
   )

{- | >>> clampToByte (-23)
0
>>> clampToByte 17.3
18
>>> clampToByte 300
31
-}
clampToByte :: Double -> Word8
clampToByte = ceiling . min 0b1_1111 . max 0
