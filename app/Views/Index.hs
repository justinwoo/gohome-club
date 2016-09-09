{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Index (index) where

import           Prelude                       hiding (div, span)

import           Control.Lens                  hiding (index)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              hiding (html, style)
import           Text.Blaze.Html5.Attributes   hiding (item, span)
import           Web.Scotty                    (ActionM, html)

import           Models.Weather
import           Views.Layout                  (layout)

weatherHtml :: Weather -> Html
weatherHtml weather = do
  let channel' = weather ^. queryL . resultsL . channelL
  let location' = channel' ^. locationL
  let astronomy' = channel' ^. astronomyL
  let condition' = channel' ^. itemL . conditionL
  let wind' = channel' ^. windL
  div ! style "display: inline-block" $ do
    h2 $ toHtml $ "Weather information for " ++ location' ^. cityL ++ ", " ++ location' ^. countryL
    ul $ do
      li $ toHtml $ "Current temperature: " ++ condition' ^. conditiontempL
      li $ toHtml $ "Current condition: " ++ condition' ^. conditiontextL
      li $ toHtml $ "Sunrise: " ++ astronomy' ^. sunriseL
      li $ toHtml $ "Sunset: " ++ astronomy' ^. sunsetL
      li $ toHtml $ "Wind chill: " ++ wind' ^. chillL
      li $ toHtml $ "Wind direction: " ++ wind' ^. directionL
      li $ toHtml $ "Wind speed: " ++ wind' ^. speedL
    h2 "10-Day Forecast:"
    ul $ mconcat $
      (\x ->
        li $ do
          toHtml $ x ^. forecastdayL
          text " "
          toHtml $ x ^. forecastdateL
          text ": "
          toHtml $ x ^. forecasthighL
          text ", "
          toHtml $ x ^. forecastlowL
          text " - "
          toHtml $ x ^. forecasttextL
        ) <$> channel' ^. itemL . forecastL

index :: [Weather] -> ActionM ()
index xs = html . renderHtml $ layout "This page was found" $ do
  h1 "The page was found"
  text "The page you were looking for might be here, had its name changed, or is temporarily unavailable."
  hr
  p "Please try the following:"
  ul $ do
    li "Make sure that the Web site address displayed in the address bar of your browser is spelled and formatted correctly."
    li "If you reached this page by clicking a link, contact the Web site administrator to alert them that the link may be incorrect."
    li $ do
      text "Click the "
      a ! href "javascript:history.back(1)" $ "Back"
      text " button to try another link."
  h2 $ do
    text "HTTP Error 200 - File was found."
    br
    text "The World Wide Web (WWW)"
  hr
  p "Technical Information (for support personnel)"
  ul $ do
    li $ do
      text "Go to "
      a ! href "http://go.microsoft.com/fwlink/?linkid=8180" $ "Microsoft Product Support Services"
      text " and perform a title search for the words "
      b "HTTP"
      text " and "
      b "404"
      text "."
    li $ do
      text "Open "
      b "IIS Help"
      text ", which is accessible in IIS Manager (inetmgr), and search for topics titled "
      b "Web Site Setup"
      text ", "
      b "Common Administrative Tasks"
      text ", and "
      b "About Custom Error Messages"
      text "."
  p "Page contents continue below."
  hr

  span $ mconcat $ weatherHtml <$> xs

  hr
  h2 "Links to author on other websites"
  p $
    ul $ do
      li $ a ! href "https://twitter.com/jusrin00" $ "Twitter"
      li $ a ! href "https://github.com/justinwoo" $ "Github"
      li $ a ! href "http://qiita.com/kimagure" $ "Qiita"
      li $ a ! href "https://speakerdeck.com/justinwoo" $ "Speakerdeck"
