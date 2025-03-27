{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.List (sortBy, words, lines)
import Data.Ord (comparing)
import System.IO (hFlush, stdout)
import Control.Monad (forever)
import System.CPUTime (getCPUTime)
import System.Process (readCreateProcess, shell)

-- | Count words, sentences, paragraphs, and unique words
countWords :: Text -> (Int, Int, Int, Int, M.Map Text Int)
countWords text =
    let wordsList = T.words text
        uniqueWords = M.fromListWith (+) [(word, 1) | word <- wordsList, T.length word > 3] -- Ignore short words
        sentences = length (T.splitOn "." text)
        paragraphs = length (T.splitOn "\n\n" text)
    in (length wordsList, sentences, paragraphs, M.size uniqueWords, uniqueWords)

-- | Compute readability score (Flesch-Kincaid formula approximation)
readabilityScore :: Int -> Int -> Int -> Double
readabilityScore wordsCount sentencesCount syllablesCount
    | sentencesCount == 0 || wordsCount == 0 = 0  -- Avoid division by zero
    | otherwise =
        206.835 - (1.015 * (fromIntegral wordsCount / fromIntegral sentencesCount))
                 - (84.6 * (fromIntegral syllablesCount / fromIntegral wordsCount))

-- | Simple Sentiment Analysis
sentimentAnalysis :: Text -> String
sentimentAnalysis text
    | any (`T.isInfixOf` text) ["happy", "great", "love", "excellent", "good"] = "Positive"
    | any (`T.isInfixOf` text) ["bad", "sad", "terrible", "hate", "angry"] = "Negative"
    | otherwise = "Neutral"

-- | Extract Keywords (Most frequent words, ignoring common words)
extractKeywords :: M.Map Text Int -> [(Text, Int)]
extractKeywords freqMap =
    take 5 (sortBy (comparing (negate . snd)) (M.toList freqMap))

-- | Estimate reading time in minutes
estimateReadingTime :: Int -> Double
estimateReadingTime wordCount = fromIntegral wordCount / 200  -- Assuming 200 WPM reading speed

-- | Sentence-to-Word Ratio
sentenceToWordRatio :: Int -> Int -> Double
sentenceToWordRatio wordCount sentenceCount
    | sentenceCount == 0 = 0  -- Avoid division by zero
    | otherwise = fromIntegral wordCount / fromIntegral sentenceCount

-- | Words Per Minute Tracker
wpmTracker :: IO ()
wpmTracker = do
    putStrLn "Start typing below. Press ENTER when done:"
    hFlush stdout
    start <- getCPUTime
    input <- getLine
    end <- getCPUTime
    let elapsedTime = fromIntegral (end - start) / 1e12  -- Convert picoseconds to seconds
        wordCount = length (words input)
        wpm = (fromIntegral wordCount / elapsedTime) * 60
    putStrLn $ "Words per minute: " ++ show (round wpm)

-- | Speech-to-Text using PowerShell
speechToText :: IO Text
speechToText = do
    putStrLn "Recording... Speak now!"
    hFlush stdout
    let command = "powershell -Command \"Add-Type -AssemblyName System.Speech; $s=New-Object System.Speech.Recognition.SpeechRecognitionEngine; $s.SetInputToDefaultAudioDevice(); $s.LoadGrammar([System.Speech.Recognition.DictationGrammar]::new()); $s.Recognize().Text\""
    result <- readCreateProcess (shell command) ""
    putStrLn "Processing speech..."
    return (T.strip (T.pack result))

-- | Main Function
main :: IO ()
main = do
    putStrLn "Welcome to Text Analyzer"
    putStrLn "1. Enter text manually"
    putStrLn "2. Load from a file"
    putStrLn "3. Use speech-to-text"
    putStr "Enter option (1, 2, or 3): "
    hFlush stdout
    option <- getLine

    case option of
        "1" -> do
            putStrLn "Start typing. Press ENTER when done:"
            text <- TIO.getLine
            analyzeText text
        "2" -> do
            putStr "Enter file path: "
            hFlush stdout
            filePath <- getLine
            text <- TIO.readFile filePath
            analyzeText text
        "3" -> do
            text <- speechToText
            analyzeText text
        _ -> putStrLn "Invalid option."

-- | Analyze Text and Display Results
analyzeText :: Text -> IO ()
analyzeText text = do
    let (wordsCount, sentencesCount, paragraphsCount, uniqueWordsCount, wordFreq) = countWords text
        syllablesCount = wordsCount `div` 3  -- Rough estimation
        readability = readabilityScore wordsCount sentencesCount syllablesCount
        sentiment = sentimentAnalysis text
        keywords = extractKeywords wordFreq
        readingTime = estimateReadingTime wordsCount
        wordRatio = sentenceToWordRatio wordsCount sentencesCount

    putStrLn "\nText Analysis Results"
    putStrLn $ "Total Words: " ++ show wordsCount
    putStrLn $ "Sentences: " ++ show sentencesCount
    putStrLn $ "Paragraphs: " ++ show paragraphsCount
    putStrLn $ "Unique Words: " ++ show uniqueWordsCount
    putStrLn $ "Readability Score: " ++ show readability
    putStrLn $ "Sentiment Analysis: " ++ sentiment
    putStrLn $ "Top Keywords: " ++ show keywords
    putStrLn $ "Estimated Reading Time: " ++ show readingTime ++ " minutes"
    putStrLn $ "Sentence-to-Word Ratio: " ++ show wordRatio

    putStrLn "\nNow, let's measure your typing speed!"
    wpmTracker
