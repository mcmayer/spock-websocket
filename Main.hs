{-# LANGUAGE OverloadedStrings, TypeOperators, DataKinds #-}
module Main where

import           Web.Spock as Sp
import           Web.Spock.Core (SpockCtxT)
import           Web.Spock.Config
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (void)
import           Control.Exception (AsyncException, fromException, throwIO, handle)
import           Control.Concurrent (forkIO, threadDelay)
import           Network.Wai.Handler.WebSockets
import           Network.Wai (Application, Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static
import           Network.WebSockets
import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import           Text.Blaze.Html5                     (dataAttribute, h1, h2,
                                                       h3, h4, h5, img, li,
                                                       link, meta, nav, ul, (!))
import qualified Text.Blaze.Html5                     as H
import           Text.Blaze.Html5.Attributes          (alt, class_, height,
                                                       href, role, src, style,
                                                       target, width)
import qualified Text.Blaze.Html5.Attributes          as A
import           Debug.Trace (trace)

tr :: Show a => a -> a
tr a = trace (show a) a

data MySession = EmptySession
type MyAppState = ()

main :: IO ()
main = do
    spockConfig <- defaultSpockCfg EmptySession PCNoDatabase ()
    runSpock 8080 (spock spockConfig (appMiddleware >> app))

appMiddleware :: Web.Spock.Core.SpockCtxT () (WebStateM () MySession MyAppState) ()
appMiddleware = do
    middleware logStdoutDev
    middleware (staticPolicy $ noDots >-> addBase "static")
    middleware wsMiddleware

wsMiddleware :: Middleware
wsMiddleware =  websocketsOr defaultConnectionOptions wsApp
    where
        wsApp :: ServerApp
        wsApp pendingConn = do
            conn <- acceptRequest pendingConn
            forkPingThread conn 30
            sendTextData conn ("..." :: Text)
            go conn 1
        go :: Connection -> Int -> IO ()
        go conn i = do
            threadDelay 150000   -- 50ms
            sendTextData conn (T.pack $ show i)
            go conn (i + 1)
        
app :: SpockM () MySession MyAppState ()
app = get root action

content :: H.Html
content = 
    H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
            meta ! A.charset "utf-8"
            H.title "Spock Websockets"
            H.script ! A.type_ "text/javascript" ! src "ws.js" $ mempty
            link ! A.type_ "text/css" ! A.rel "stylesheet" ! href "ws.css"
        H.body $ do
            h1 "Spock Websockets"
            H.p $ do
                "Counter: "
                H.span ! A.id "counter" $ mempty
            H.p ! A.id "boxes1" ! A.class_ "boxes" $ mempty
            H.p ! A.id "boxes10" ! A.class_ "boxes" $ mempty

action :: ActionCtxT ctx (WebStateM () MySession MyAppState) a
action = (Sp.html . TL.toStrict . renderHtml) content

    