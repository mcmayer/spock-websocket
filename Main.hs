{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Exception                    (AsyncException,
                                                       fromException, handle,
                                                       throwIO)
import           Control.Monad                        (void)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Network.Wai                          (Application, Middleware)
import           Network.Wai.Handler.WebSockets
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static
import           Network.WebSockets
import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import           Text.Blaze.Html5                     ((!))
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
import           Web.Spock                            as Sp
import           Web.Spock.Config                     as Sp
import           Web.Spock.Core                       (SpockCtxT)

data MySession = EmptySession
type MyAppState = ()

main :: IO ()
main = do
    spockConfig <- defaultSpockCfg EmptySession PCNoDatabase ()
    runSpock 8080 (spock spockConfig (appMiddlewares >> app))

appMiddlewares :: Web.Spock.Core.SpockCtxT () (WebStateM () MySession MyAppState) ()
appMiddlewares = do
    middleware logStdoutDev                                     -- add logging
    middleware (staticPolicy $ noDots >-> addBase "static")     -- add static files
    middleware wsMiddleware                                     -- add websocket

wsMiddleware :: Middleware
wsMiddleware =  websocketsOr defaultConnectionOptions wsApp
    where
        wsApp :: ServerApp
        wsApp pendingConn = do
            conn <- acceptRequest pendingConn
            forkPingThread conn 30  -- not needed here, but good to have in most other situations
            sendTextData conn ("..." :: Text)
            counter conn 1
        counter :: Connection -> Int -> IO ()
        counter conn i = do
            threadDelay 50000   -- 50ms
            sendTextData conn (T.pack $ show i)
            counter conn (i + 1)

app :: SpockM () MySession MyAppState ()
app = get root action   -- there's only one route in this app

content :: H.Html
content =
    H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
            H.meta ! A.charset "utf-8"
            H.title "Spock Websockets"
            H.script ! A.type_ "text/javascript" ! A.src "ws.js" $ mempty
            H.link ! A.type_ "text/css" ! A.rel "stylesheet" ! A.href "ws.css"
        H.body $ do
            H.h1 "Spock Websockets"
            H.p $ do
                "Counter: "
                H.span ! A.id "counter" $ mempty
            H.p ! A.id "boxes1" ! A.class_ "boxes" $ mempty
            H.p ! A.id "boxes10" ! A.class_ "boxes" $ mempty

action :: ActionCtxT ctx (WebStateM () MySession MyAppState) a
action = (Sp.html . TL.toStrict . renderHtml) content