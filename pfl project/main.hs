{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text.Lazy (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad.IO.Class (liftIO)

-- | Stopwords List
stopwords :: [T.Text]
stopwords = ["the", "a", "an", "in", "on", "and", "or", "is", "I", "you", "it", "to", "with", "for", "of", "this", "that", "he", "she", "we", "they", "his", "her", "their", "was", "were", "be", "been", "am", "are", "as", "by", "at"]

positiveWords, negativeWords :: [T.Text]
positiveWords = ["happy", "joy", "great", "good", "love", "excellent", "awesome", "fantastic", "wonderful", "positive", "success", "brilliant", "amazing", "superb"]
negativeWords = ["sad", "bad", "terrible", "awful", "worst", "hate", "horrible", "negative", "angry", "depressing", "failure", "disaster", "painful", "miserable"]

sentimentAnalysis :: [T.Text] -> (String, Double)
sentimentAnalysis wordsList =
    let posCount = length [w | w <- wordsList, T.toLower w `elem` positiveWords]
        negCount = length [w | w <- wordsList, T.toLower w `elem` negativeWords]
        totalCount = posCount + negCount
        confidence = if totalCount == 0 then 0.0 else fromIntegral (max posCount negCount) / fromIntegral totalCount * 100
        sentiment = case compare posCount negCount of
            GT -> "😊 Positive Sentiment"
            LT -> "😡 Negative Sentiment"
            EQ -> "😐 Neutral Sentiment"
    in (sentiment, confidence)

wordFrequency :: [T.Text] -> M.Map T.Text Int
wordFrequency wordsList =
    M.fromListWith (+) [(T.toLower word, 1) | word <- wordsList, not (word `elem` stopwords)]

extractKeywords :: M.Map T.Text Int -> [(T.Text, Int)]
extractKeywords freqMap =
    take 5 (sortBy (comparing (negate . snd)) (M.toList freqMap))

readabilityScore :: Int -> Int -> Double
readabilityScore wordsCount sentencesCount =
    if sentencesCount == 0 || wordsCount == 0 then 0
    else max 0 (206.835 - (1.015 * fromIntegral wordsCount / fromIntegral sentencesCount)
                - (84.6 * fromIntegral wordsCount / fromIntegral (max 1 (wordsCount `div` 5))))

estimateReadingTime :: Int -> Double
estimateReadingTime wordsCount =
    let avgReadingSpeed = 200.0
    in if wordsCount < 200 then 1 else fromIntegral wordsCount / avgReadingSpeed

sentenceToWordRatio :: Int -> Int -> Double
sentenceToWordRatio wordsCount sentencesCount =
    if sentencesCount == 0 then fromIntegral wordsCount else fromIntegral wordsCount / fromIntegral sentencesCount

countWords :: T.Text -> (Int, Int, Int, Int, M.Map T.Text Int)
countWords text =
    let wordsList = T.words text
        uniqueWords = wordFrequency wordsList
        sentences = T.splitOn "." text
        paragraphs = length (T.splitOn "\n\n" text)
    in (length wordsList, length sentences, paragraphs, M.size uniqueWords, uniqueWords)

main :: IO ()
main = scotty 3000 $ do
    post "/analyze" $ do
        body <- jsonData :: ActionM (M.Map String String)
        let inputText = T.pack (body M.! "text")
            wordsList = T.words inputText
            (wordsCount, sentencesCount, paragraphsCount, uniqueWordsCount, wordFreq) = countWords inputText
            readability = readabilityScore wordsCount sentencesCount
            readingTime = estimateReadingTime wordsCount
            sentenceWordRatio = sentenceToWordRatio wordsCount sentencesCount
            (sentiment, confidence) = sentimentAnalysis wordsList
            keywords = extractKeywords wordFreq

            result = unlines
                [ "📊 Text Analysis Results 📊"
                , "Total Words: " ++ show wordsCount
                , "Sentences: " ++ show sentencesCount
                , "Paragraphs: " ++ show paragraphsCount
                , "Unique Words: " ++ show uniqueWordsCount
                , "Readability Score: " ++ show readability
                , "⏳ Estimated Reading Time: " ++ show (round readingTime) ++ " minutes"
                , "📏 Sentence-to-Word Ratio: " ++ show sentenceWordRatio
                , "💬 Sentiment Analysis: " ++ sentiment ++ " (Confidence: " ++ show confidence ++ "%)"
                , "🔑 Top Keywords: " ++ show keywords
                ]
        text (pack result)
