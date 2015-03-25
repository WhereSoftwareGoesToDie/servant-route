{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Description:  servers for Servant APIs.
module Servant.Route (
  Decision(..),
  IsRouted(..),
  routingServer,
) where

import Data.Aeson hiding (Error)
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String.Conversions (cs)
import qualified Data.Text as T
import GHC.TypeLits
import Network.HTTP.Types hiding (Header (..), Status (..))
import Network.Wai
import Servant.API
import Servant.Common.Text
import qualified Servant.Server.Internal as S

-- | Use 'IsRouted' instances to
--
-- > app :: Application
-- > app = routingServer api server
routingServer
    :: (IsRouted api, S.HasServer api)
    => Proxy api
    -> S.Server api
    -> Application
routingServer p server = S.toApplication (S.route p server)

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

type API = ("a" :> "b" :> QueryParam "poop" Int :> Get Int)
    :<|> (Capture "poopin" String :> QueryParam "inthe" String :> Post String)
    :<|> ("c" :> (Capture "cat" String :<|> Capture "dog" Int) :> Delete)

-- | Perform routing according to normal rules for mapping URIs to \"physical\"
-- resources.
--
-- The 'match' method inspects the 'Request' to determine whether it should be
-- handled by the @layout@ resource. This idea is to check /only/ that this is
-- the correct handler, /not/ that the request is correct and complete. This
-- ensures that we'll call a specific 'HasServer' instance which can return
-- meaningful errors if required.
class (S.HasServer layout) => IsRouted layout where
    -- | Check that a request matches this resource.
    --
    -- Essentially: "Is this the physical resource that should handle the
    -- request?"
    match :: Proxy layout -> Predicate

instance (IsRouted first, IsRouted rest, S.HasServer first, S.HasServer rest)
        => IsRouted (first :<|> rest) where

    match p req = match (Proxy :: Proxy first) req
                <> match (Proxy :: Proxy rest) req

instance (KnownSymbol path, IsRouted sublayout) => IsRouted (path :> sublayout) where
    match _ req = case S.processedPathInfo req of
            (first : rest) | first == cs (symbolVal proxyPath) ->
                match (Proxy :: Proxy sublayout) req{ pathInfo = rest }
            _ -> reject Nothing
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

-- | Check the method.
instance (ToJSON result) => IsRouted (Get result) where
    match _ = matchMethod methodGet

-- | Check the method.
instance (ToJSON result) => IsRouted (Put result) where
    match _ = matchMethod methodPut

-- | Check the method.
instance (ToJSON result) => IsRouted (Post result) where
    match _ = matchMethod methodPost

-- | Check the method.
instance IsRouted Delete where
    match _ = matchMethod methodDelete

-- | Leave all routing decisions up to a 'Raw' endpoint.
instance IsRouted Raw where
    match _ _ = accept

-- | Check that the request has an acceptable HTTP method.
matchMethod
    :: Method
    -> Predicate
matchMethod method req =
    if requestMethod req == method
        then accept
        else reject $ Just (405, "Method Not Allowed")

-- | Ignore when routing.
instance (KnownSymbol sym, FromText a, IsRouted sublayout)
        => IsRouted (Header sym a :> sublayout) where

    match _ = match (Proxy :: Proxy sublayout)

-- | Ignore when routing.
instance (FromJSON a, IsRouted sublayout)
        => IsRouted (ReqBody a :> sublayout) where

    match _ = match (Proxy :: Proxy sublayout)

-- | Ignore when routing.
instance (KnownSymbol sym, FromText a, IsRouted sublayout)
        => IsRouted (QueryParam sym a :> sublayout) where

    match _ = match (Proxy :: Proxy sublayout)

-- | Ignore when routing.
instance (KnownSymbol sym, FromText a, IsRouted sublayout)
        => IsRouted (QueryParams sym a :> sublayout) where

    match _ = match (Proxy :: Proxy sublayout)

-- | Ignore when routing.
instance (KnownSymbol sym, IsRouted sublayout)
        => IsRouted (QueryFlag sym :> sublayout) where

    match _ = match (Proxy :: Proxy sublayout)
