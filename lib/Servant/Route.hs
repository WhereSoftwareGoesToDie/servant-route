{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

-- | Description:  servers for Servant APIs.
module Servant.Route (
  Decision,
  IsRouted(..)
) where

import Data.ByteString (ByteString)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String.Conversions (cs)
import qualified Data.Text as T
import GHC.TypeLits
import Network.HTTP.Types hiding (Status (..), Header(..))
import Network.Wai
import Servant.API
import Servant.Common.Text
import qualified Servant.Server.Internal as S

type Status = (Int, ByteString)

-- | Should the handler corresponding to a predicate be executed?
data Decision
    = RunHandler          -- ^ Yes, run the handler.
    | Error Status        -- ^ Yes, but the request is bad, return an error.
    | Skip (Maybe Status) -- ^ No, try the next handler.

instance Monoid Decision where
    mempty = Skip Nothing

    e@(Error _) `mappend` _ = e
    s@(Skip _) `mappend` _ = s
    RunHandler `mappend` r = r

accept :: Decision
accept = RunHandler

reject :: Maybe (Int, ByteString) -> Decision
reject = Skip

invalid :: (Int, ByteString) -> Decision
invalid = Error

-- | TODO: Make correct.
type Predicate = Request -> Decision

class IsRouted layout where
    -- | Check that a request matches this resource.
    --
    -- Essentially: "Is this the physical resource that should handle the
    -- request?"
    match :: Proxy layout -> Predicate

    -- | Check that a request is valid for this resource.
    --
    -- Essentially: "Do the inputs make this physical resource happy?"
    validate :: Proxy layout -> Predicate

instance (KnownSymbol path, IsRouted sublayout) => IsRouted (path :> sublayout) where
    match _ req = case S.processedPathInfo req of
            (first : rest) | first == cs (symbolVal proxyPath) ->
                match (Proxy :: Proxy sublayout) req{ pathInfo = rest }
            _ -> reject Nothing
      where
        proxyPath = Proxy :: Proxy path

    validate _ req = case S.processedPathInfo req of
            (first : rest) | first == cs (symbolVal proxyPath) ->
                validate (Proxy :: Proxy sublayout) req{ pathInfo = rest }
            _ -> invalid (404, "Not Found")
      where
        proxyPath = Proxy :: Proxy path

-- | Check the patch contains a value for a 'Capture' and that it's 
instance (KnownSymbol capture, FromText a, IsRouted sublayout)
    => IsRouted (Capture capture a :> sublayout) where

    -- Check that there is a value in the path corresponding to the 'Capture'.
    match _ req = case S.processedPathInfo req of
            (first : rest) | (not . T.null $ first) ->
                match (Proxy :: Proxy sublayout) req{ pathInfo = rest }
            _ -> reject Nothing

    -- Check that the value can be read as an 'a'.
    validate _ req = case S.processedPathInfo req of
            (first : rest) | isJust (S.captured captureProxy first) ->
                validate (Proxy :: Proxy sublayout) req{ pathInfo = rest }
            _ -> invalid (404, "Not Found")
      where
        captureProxy = Proxy :: Proxy (Capture capture a)


-- | Check the request method is GET.
instance IsRouted (Get result) where
    match _ = matchMethod methodGet
    validate _ _ = accept

-- | Check the request method is PUT.
instance IsRouted (Put result) where
    match _ = matchMethod methodPut
    validate _ _ = accept

-- | Check the request method is POST.
instance IsRouted (Post result) where
    match _ = matchMethod methodPost
    validate _ _ = accept

-- | Check the request method is DELETE.
instance IsRouted Delete where
    match _ = matchMethod methodDelete
    validate _ _ = accept

-- | Leave all routing decisions up to a 'Raw' endpoint.
instance IsRouted Raw where
    match _ _ = accept
    validate _ _ = accept

-- | Check that the request has an acceptable HTTP method.
matchMethod
    :: Method
    -> Predicate
matchMethod method req =
    if requestMethod req == method
        then accept
        else reject $ Just (405, "Method Not Allowed")

-- | Request headers do not affect routing.
instance (KnownSymbol sym, IsRouted sublayout)
        => IsRouted (Header sym a :> sublayout) where

    match _ = match (Proxy :: Proxy sublayout)
    validate _ = validate (Proxy :: Proxy sublayout)

-- | The request body does not affect routing.
instance (IsRouted sublayout) => IsRouted (ReqBody a :> sublayout) where
    match _ = match (Proxy :: Proxy sublayout)
    validate _ = validate (Proxy :: Proxy sublayout)

-- | A query parameter does not affect routing.
instance (KnownSymbol sym, IsRouted sublayout)
        => IsRouted (QueryParam sym a :> sublayout) where

    match _ = match (Proxy :: Proxy sublayout)
    validate _ = validate (Proxy :: Proxy sublayout)

-- | Query parameters do not affect routing.
instance (KnownSymbol sym, IsRouted sublayout)
        => IsRouted (QueryParams sym a :> sublayout) where

    match _ = match (Proxy :: Proxy sublayout)
    validate _ = validate (Proxy :: Proxy sublayout)

-- | A query flag does not affect routing.
instance (KnownSymbol sym, IsRouted sublayout)
        => IsRouted (QueryFlag sym :> sublayout) where

    match _ = match (Proxy :: Proxy sublayout)
    validate _ = validate (Proxy :: Proxy sublayout)
