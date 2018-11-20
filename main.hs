{-# LANGUAGE OverloadedStrings #-}
import           CedictTools
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import           Data.Trie (Trie)
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Trie as Trie
import           System.IO
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as Renderer

data Span = Span {
    spanText :: Text,
    spanEntries :: [CedictEntry]
}

maybeSpan :: Text -> [CedictEntry] -> Maybe Span
maybeSpan text _ | Text.null text = Nothing
maybeSpan text entries = Just $ Span text entries

process :: CedictTools -> Text -> [Span]
process tools text =
    process_helper tools text Text.empty

process_helper :: CedictTools -> Text -> Text -> [Span]
process_helper tools text cur | Text.null text = maybeToList $ maybeSpan cur []
process_helper tools text cur =
    case match tools text of
        Nothing ->
            process_helper tools (Text.tail text) (Text.snoc cur (Text.head text))
        Just ((consumed, entries), remaining) ->
            let
                curSpan = maybeToList $ maybeSpan cur []
                consumedSpan = Span consumed entries
            in
                curSpan ++ [consumedSpan] ++ process tools remaining

toParagraphs :: [Span] -> [[Span]]
toParagraphs spans =
    let
        piece :: [Span] -> Maybe ([Span], [Span])
        piece spans =
            case break (isJust . Text.find (== '\n') . spanText) spans of
                ([], []) ->
                    Nothing
                (pre, (Span text entry):post) ->
                    let
                        (textPre, textPost) = Text.breakOn "\n" text
                        rest = Text.tail textPost
                    in
                        if Text.null rest then
                            Just (pre ++ [Span textPre entry], post)
                        else
                            Just (pre ++ [Span textPre entry], (Span rest entry):post)
                (pre, _) ->
                    Just (pre, [])
    in
        unfoldr piece spans

tooltipHtml span =
    forM_ (spanEntries span) $ \entry ->
        H.p $ do
            H.toHtml (showEntry entry)

doc :: CedictTools -> Text -> Html
doc tools text = text `seq` H.docTypeHtml $
    let
        spans = process tools text
        paragraphs = toParagraphs $ spans
        spansWithEntries = filter (not . null . spanEntries) spans
        entryMap = Map.fromList (map (\(Span text entries) -> (text, entries)) spansWithEntries)
    in do
        H.head $ do
            H.title "Flowlike Chinese Reader"
            H.link ! A.href "style.css" ! A.rel "stylesheet" ! A.type_ "text/css"
        H.body $ H.div ! A.id "content" $ do
            H.div ! A.id "text" $ do
                forM_ paragraphs $ \p ->
                    H.p $ forM_ p $ \span ->
                        H.span $ H.toHtml $ spanText span
                H.div ! A.id "tooltips" $ forM_ (Map.toList entryMap) $ \(text, entries) ->
                    H.div ! A.id (H.textValue $ Text.append "tt_" text) $
                        tooltipHtml $ Span text entries
            H.script ! A.src "tooltip.js" $ mempty

test :: CedictTools -> Text -> IO ()
test tools text =
    putStrLn $ Renderer.renderHtml $ doc tools text

cedictFile = "cedict_1_0_ts_utf-8_mdbg.txt"

main =
    do
        tools <- initCedictTools cedictFile
        input <- getContents
        putStrLn $ Renderer.renderHtml $ doc tools (Text.pack input)
