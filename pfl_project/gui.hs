{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- | Count words, sentences, paragraphs, and unique words
countWords :: T.Text -> (Int, Int, Int, Int, M.Map T.Text Int)
countWords text =
    let wordsList = T.words text
        uniqueWords = M.fromListWith (+) [(word, 1) | word <- wordsList, T.length word > 3]
        sentences = length (T.splitOn "." text)
        paragraphs = length (T.splitOn "\n\n" text)
    in (length wordsList, sentences, paragraphs, M.size uniqueWords, uniqueWords)

-- | Readability Score
readabilityScore :: Int -> Int -> Int -> Double
readabilityScore wordsCount sentencesCount syllablesCount
    | sentencesCount == 0 || wordsCount == 0 = 0
    | otherwise =
        206.835 - (1.015 * (fromIntegral wordsCount / fromIntegral sentencesCount))
                 - (84.6 * (fromIntegral syllablesCount / fromIntegral wordsCount))

-- | Simple Sentiment Analysis
sentimentAnalysis :: T.Text -> String
sentimentAnalysis text
    | any (`T.isInfixOf` text) ["happy", "great", "love", "excellent", "good"] = "Positive"
    | any (`T.isInfixOf` text) ["bad", "sad", "terrible", "hate", "angry"] = "Negative"
    | otherwise = "Neutral"

-- | Extract Keywords (Most frequent words)
extractKeywords :: M.Map T.Text Int -> [(T.Text, Int)]
extractKeywords freqMap =
    take 5 (sortBy (comparing (negate . snd)) (M.toList freqMap))

-- | Start the GUI
main :: IO ()
main = startGUI defaultConfig { jsPort = Just 8023 } setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Haskell Text Analyzer"

    -- UI Elements
    inputBox <- UI.textarea # set (attr "placeholder") "Enter text here" # set style [("width", "500px"), ("height", "100px")]
    analyzeButton <- UI.button # set text "Analyze"
    outputBox <- UI.div

    -- Button Event
    on UI.click analyzeButton $ \_ -> do
        text <- get value inputBox
        let (wordsCount, sentencesCount, paragraphsCount, uniqueWordsCount, wordFreq) = countWords (T.pack text)
            syllablesCount = wordsCount `div` 3
            readability = readabilityScore wordsCount sentencesCount syllablesCount
            sentiment = sentimentAnalysis (T.pack text)
            keywords = extractKeywords wordFreq

            resultText = UI.div # set html (unlines
                         [ "<b>Total Words:</b> " ++ show wordsCount
                         , "<b>Sentences:</b> " ++ show sentencesCount
                         , "<b>Paragraphs:</b> " ++ show paragraphsCount
                         , "<b>Unique Words:</b> " ++ show uniqueWordsCount
                         , "<b>Readability Score:</b> " ++ show readability
                         , "<b>Sentiment Analysis:</b> " ++ sentiment
                         , "<b>Top Keywords:</b> " ++ show keywords
                         ])

        element outputBox # set children [resultText]

    -- Layout
    getBody window #+ [UI.h1 # set text "Haskell Text Analyzer", element inputBox, element analyzeButton, element outputBox]
