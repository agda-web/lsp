{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# Language MagicHash #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeInType #-}

module Language.LSP.Protocol.Message.Types where

import Language.LSP.Protocol.Internal.Method
import Language.LSP.Protocol.Message.LspId
import Language.LSP.Protocol.Message.Meta
import Language.LSP.Protocol.Message.Method ()
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types.Common
import Language.LSP.Protocol.Utils.Misc

import Data.Aeson hiding (Null)
import Data.Aeson qualified as J
-- import Data.Aeson.TH
---- for unsafePackLenLiteral
import GHC.Exts (Addr#, Ptr (Ptr))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short.Internal (createFromPtr)
---- other
import Data.Aeson.Key ()
import qualified Data.Aeson.Encoding
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair, Parser, JSONPathElement(Key))
import qualified Data.Aeson.Encoding.Internal
import qualified Data.DList
import Text.Printf (printf)

import Data.Kind
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits (KnownSymbol)
import Prettyprinter

-- 'RequestMessage', 'ResponseMessage', 'ResponseError', and 'NotificationMessage'
-- aren't present in the metamodel, although they should be.
-- https://github.com/microsoft/vscode-languageserver-node/issues/1079

unsafePackLenLiteral :: Int -> Addr# -> ShortByteString
unsafePackLenLiteral len addr# =
    unsafeDupablePerformIO $ createFromPtr (Ptr addr#) len

aesonPair :: (KeyValue e a, ToJSON v) => Key -> v -> Data.DList.DList a
aesonPair k v = Data.DList.singleton (k .= v)

aesonFromPairs :: Data.DList.DList Pair -> Value
aesonFromPairs = object . Data.DList.toList

aesonFromPairsEnc :: Series -> Encoding
aesonFromPairsEnc = Data.Aeson.Encoding.pairs

aesonLookupFieldOmit :: Maybe a -> (Value -> Parser a) -> String -> String -> Object -> Key -> Parser a
aesonLookupFieldOmit maybeDefault pj tName rec obj key =
    case KM.lookup key obj of
      Nothing ->
        case maybeDefault of
          Nothing -> unknownFieldFail tName rec (Key.toString key)
          Just x -> pure x
      Just v  -> pj v <?> Key key

unknownFieldFail :: String -> String -> String -> Parser fail
unknownFieldFail tName rec key =
    fail $ printf "When parsing the record %s of type %s the key %s was not present."
                  rec tName key

aesonParseTypeMismatch' :: String -> String -> String -> String -> Parser fail
aesonParseTypeMismatch' conName_ tName expected actual =
    fail $ printf "When parsing the constructor %s of type %s expected %s but got %s."
                  conName_ tName expected actual

aesonValueConName :: Value -> String
aesonValueConName _ = "N/A"

-- | Notification message type as defined in the spec.
data NotificationMessage = NotificationMessage
  { _jsonrpc :: Text
  , _method :: Text
  , _params :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

-- deriveJSON lspOptions ''NotificationMessage
instance ToJSON NotificationMessage where
  toJSON
    = let
      in
        \ value_aUG1
          -> case value_aUG1 of
               NotificationMessage arg1_aUG4 arg2_aUG5 arg3_aUG6
                 -> aesonFromPairs
                      ((if omitField arg1_aUG4 then
                            mempty
                        else
                            aesonPair
                              (Key.fromString "jsonrpc") (toJSON arg1_aUG4))
                         <>
                           ((if omitField arg2_aUG5 then
                                 mempty
                             else
                                 aesonPair
                                   (Key.fromString "method") (toJSON arg2_aUG5))
                              <>
                                (if omitField arg3_aUG6 then
                                     mempty
                                 else
                                     aesonPair
                                       (Key.fromString "params")
                                       (toJSON arg3_aUG6))))
  toEncoding
    = let
        _let0_aUGh
          = unsafePackLenLiteral
              10 "\"jsonrpc\":"#
        _let1_aUGi
          = unsafePackLenLiteral
              9 "\"method\":"#
        _let2_aUGl
          = unsafePackLenLiteral
              9 "\"params\":"#
      in
        \ value_aUGb
          -> case value_aUGb of
               NotificationMessage arg1_aUGe arg2_aUGf arg3_aUGg
                 -> aesonFromPairsEnc
                      ((if omitField arg1_aUGe then
                            mempty
                        else
                            Data.Aeson.Encoding.Internal.unsafePairSBS
                              _let0_aUGh (toEncoding arg1_aUGe))
                         <>
                           ((if omitField arg2_aUGf then
                                 mempty
                             else
                                 Data.Aeson.Encoding.Internal.unsafePairSBS
                                   _let1_aUGi (toEncoding arg2_aUGf))
                              <>
                                (if omitField arg3_aUGg then
                                     mempty
                                 else
                                     Data.Aeson.Encoding.Internal.unsafePairSBS
                                       _let2_aUGl (toEncoding arg3_aUGg))))
instance FromJSON NotificationMessage where
  parseJSON
    = \ value_aUGo
        -> case value_aUGo of
             Object recObj_aUGp
               -> (((NotificationMessage
                       <$>
                         aesonLookupFieldOmit
                           omittedField parseJSON
                           "Language.LSP.Protocol.Message.Types.NotificationMessage"
                           "NotificationMessage" recObj_aUGp
                           (Key.fromString "jsonrpc"))
                      <*>
                        aesonLookupFieldOmit
                          omittedField parseJSON
                          "Language.LSP.Protocol.Message.Types.NotificationMessage"
                          "NotificationMessage" recObj_aUGp
                          (Key.fromString "method"))
                     <*>
                       aesonLookupFieldOmit
                         omittedField parseJSON
                         "Language.LSP.Protocol.Message.Types.NotificationMessage"
                         "NotificationMessage" recObj_aUGp
                         (Key.fromString "params"))
             other_aUGw
               -> aesonParseTypeMismatch'
                    "NotificationMessage"
                    "Language.LSP.Protocol.Message.Types.NotificationMessage" "Object"
                    (aesonValueConName other_aUGw)
deriving via ViaJSON NotificationMessage instance Pretty NotificationMessage

-- This isn't present in the metamodel.

-- | Request message type as defined in the spec.
data RequestMessage = RequestMessage
  { _jsonrpc :: Text
  , _id :: Int32 |? Text
  , _method :: Text
  , _params :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

-- deriveJSON lspOptions ''RequestMessage
instance ToJSON RequestMessage where
  toJSON
    = let
      in
        \ value_aVtk
          -> case value_aVtk of
               RequestMessage arg1_aVuC arg2_aVuE arg3_aVuF arg4_aVuG
                 -> aesonFromPairs
                      ((if omitField arg1_aVuC then
                            mempty
                        else
                            aesonPair
                              (Key.fromString "jsonrpc") (toJSON arg1_aVuC))
                         <>
                           ((if omitField arg2_aVuE then
                                 mempty
                             else
                                 aesonPair
                                   (Key.fromString "id") (toJSON arg2_aVuE))
                              <>
                                ((if omitField arg3_aVuF then
                                      mempty
                                  else
                                      aesonPair
                                        (Key.fromString "method") (toJSON arg3_aVuF))
                                   <>
                                     (if omitField arg4_aVuG then
                                          mempty
                                      else
                                          aesonPair
                                            (Key.fromString "params")
                                            (toJSON arg4_aVuG)))))
  toEncoding
    = let
        _let1_aVwx
          = unsafePackLenLiteral
              5 "\"id\":"#
        _let0_aVwg
          = unsafePackLenLiteral
              10 "\"jsonrpc\":"#
        _let2_aVwL
          = unsafePackLenLiteral
              9 "\"method\":"#
        _let3_aVwW
          = unsafePackLenLiteral
              9 "\"params\":"#
      in
        \ value_aVvA
          -> case value_aVvA of
               RequestMessage arg1_aVvZ arg2_aVw0 arg3_aVw1 arg4_aVw2
                 -> aesonFromPairsEnc
                      ((if omitField arg1_aVvZ then
                            mempty
                        else
                            Data.Aeson.Encoding.Internal.unsafePairSBS
                              _let0_aVwg (toEncoding arg1_aVvZ))
                         <>
                           ((if omitField arg2_aVw0 then
                                 mempty
                             else
                                 Data.Aeson.Encoding.Internal.unsafePairSBS
                                   _let1_aVwx (toEncoding arg2_aVw0))
                              <>
                                ((if omitField arg3_aVw1 then
                                      mempty
                                  else
                                      Data.Aeson.Encoding.Internal.unsafePairSBS
                                        _let2_aVwL (toEncoding arg3_aVw1))
                                   <>
                                     (if omitField arg4_aVw2 then
                                          mempty
                                      else
                                          Data.Aeson.Encoding.Internal.unsafePairSBS
                                            _let3_aVwW (toEncoding arg4_aVw2)))))
instance FromJSON RequestMessage where
  parseJSON
    = \ value_aVx4
        -> case value_aVx4 of
             Object recObj_aVx5
               -> ((((RequestMessage
                        <$>
                          aesonLookupFieldOmit
                            omittedField parseJSON
                            "Language.LSP.Protocol.Message.Types.RequestMessage"
                            "RequestMessage" recObj_aVx5 (Key.fromString "jsonrpc"))
                       <*>
                         aesonLookupFieldOmit
                           omittedField parseJSON
                           "Language.LSP.Protocol.Message.Types.RequestMessage"
                           "RequestMessage" recObj_aVx5 (Key.fromString "id"))
                      <*>
                        aesonLookupFieldOmit
                          omittedField parseJSON
                          "Language.LSP.Protocol.Message.Types.RequestMessage"
                          "RequestMessage" recObj_aVx5 (Key.fromString "method"))
                     <*>
                       aesonLookupFieldOmit
                         omittedField parseJSON
                         "Language.LSP.Protocol.Message.Types.RequestMessage"
                         "RequestMessage" recObj_aVx5 (Key.fromString "params"))
             other_aVxp
               -> aesonParseTypeMismatch'
                    "RequestMessage"
                    "Language.LSP.Protocol.Message.Types.RequestMessage" "Object"
                    (aesonValueConName other_aVxp)
deriving via ViaJSON RequestMessage instance Pretty RequestMessage

-- | Response error type as defined in the spec.
data ResponseError = ResponseError
  { _code :: LSPErrorCodes |? ErrorCodes
  , _message :: Text
  , _xdata :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

{- Note [ErrorCodes and LSPErrorCodes]

Confusingly, the metamodel defines _two_ enums for error codes. One of
these covers JSON RPC errors and one covers LSP-specific errors. We want
to accept either, mostly so we can make use of the pre-specified enum values.

However, _both_ of them are listed as accepting custom values. This means
that `LSPErrorCodes |? ErrorCodes` isn't quite right: when we parse it from
JSON, if we get an error code that isn't a known value of `LSPErrorCodes`, we
will just use the custom value constructor, without trying `ErrorCodes`.

It's hard to find any other good way of representing things properly with what
we've got, so in the end we decided to patch up the JSON parsing with a custom
instance.
-}
-- deriveToJSON lspOptions ''ResponseError
instance ToJSON ResponseError where
  toJSON
    = let
      in
        \ value_aVTl
          -> case value_aVTl of
               ResponseError arg1_aVTD arg2_aVTE arg3_aVTF
                 -> aesonFromPairs
                      ((if omitField arg1_aVTD then
                            mempty
                        else
                            aesonPair
                              (Key.fromString "code") (toJSON arg1_aVTD))
                         <>
                           ((if omitField arg2_aVTE then
                                 mempty
                             else
                                 aesonPair
                                   (Key.fromString "message") (toJSON arg2_aVTE))
                              <>
                                (if omitField arg3_aVTF then
                                     mempty
                                 else
                                     aesonPair
                                       (Key.fromString "data") (toJSON arg3_aVTF))))
  toEncoding
    = let
        _let0_aVUo
          = unsafePackLenLiteral
              7 "\"code\":"#
        _let2_aVUz
          = unsafePackLenLiteral
              7 "\"data\":"#
        _let1_aVUu
          = unsafePackLenLiteral
              10 "\"message\":"#
      in
        \ value_aVU0
          -> case value_aVU0 of
               ResponseError arg1_aVUg arg2_aVUi arg3_aVUj
                 -> aesonFromPairsEnc
                      ((if omitField arg1_aVUg then
                            mempty
                        else
                            Data.Aeson.Encoding.Internal.unsafePairSBS
                              _let0_aVUo (toEncoding arg1_aVUg))
                         <>
                           ((if omitField arg2_aVUi then
                                 mempty
                             else
                                 Data.Aeson.Encoding.Internal.unsafePairSBS
                                   _let1_aVUu (toEncoding arg2_aVUi))
                              <>
                                (if omitField arg3_aVUj then
                                     mempty
                                 else
                                     Data.Aeson.Encoding.Internal.unsafePairSBS
                                       _let2_aVUz (toEncoding arg3_aVUj))))
instance FromJSON ResponseError where
  parseJSON =
    let errorCode = withObject "ResponseError" $ \v ->
          ResponseError
            <$> v .: "code"
            <*> v .: "message"
            <*> v .:? "data"
     in fmap go . errorCode
   where
    go :: ResponseError -> ResponseError
    go x@(ResponseError (InL (LSPErrorCodes_Custom n)) _ _) =
      x{_code = InR (fromOpenEnumBaseType n)}
    go x = x

deriving via ViaJSON ResponseError instance Pretty ResponseError

-- | Response message type as defined in the spec.
data ResponseMessage = ResponseMessage
  { _jsonrpc :: Text
  , _id :: Int32 |? Text |? Null
  , _result :: Maybe Value
  , _error :: Maybe ResponseError
  }
  deriving stock (Show, Eq, Generic)

-- deriveJSON lspOptions ''ResponseMessage
instance ToJSON ResponseMessage where
  toJSON
    = let
      in
        \ value_aWjN
          -> case value_aWjN of
               ResponseMessage arg1_aWkc arg2_aWkd arg3_aWke arg4_aWkf
                 -> aesonFromPairs
                      ((if omitField arg1_aWkc then
                            mempty
                        else
                            aesonPair
                              (Key.fromString "jsonrpc") (toJSON arg1_aWkc))
                         <>
                           ((if omitField arg2_aWkd then
                                 mempty
                             else
                                 aesonPair
                                   (Key.fromString "id") (toJSON arg2_aWkd))
                              <>
                                ((if omitField arg3_aWke then
                                      mempty
                                  else
                                      aesonPair
                                        (Key.fromString "result") (toJSON arg3_aWke))
                                   <>
                                     (if omitField arg4_aWkf then
                                          mempty
                                      else
                                          aesonPair
                                            (Key.fromString "error")
                                            (toJSON arg4_aWkf)))))
  toEncoding
    = let
        _let3_aWlt
          = unsafePackLenLiteral
              8 "\"error\":"#
        _let1_aWle
          = unsafePackLenLiteral
              5 "\"id\":"#
        _let0_aWl9
          = unsafePackLenLiteral
              10 "\"jsonrpc\":"#
        _let2_aWln
          = unsafePackLenLiteral
              9 "\"result\":"#
      in
        \ value_aWkF
          -> case value_aWkF of
               ResponseMessage arg1_aWl0 arg2_aWl1 arg3_aWl2 arg4_aWl3
                 -> aesonFromPairsEnc
                      ((if omitField arg1_aWl0 then
                            mempty
                        else
                            Data.Aeson.Encoding.Internal.unsafePairSBS
                              _let0_aWl9 (toEncoding arg1_aWl0))
                         <>
                           ((if omitField arg2_aWl1 then
                                 mempty
                             else
                                 Data.Aeson.Encoding.Internal.unsafePairSBS
                                   _let1_aWle (toEncoding arg2_aWl1))
                              <>
                                ((if omitField arg3_aWl2 then
                                      mempty
                                  else
                                      Data.Aeson.Encoding.Internal.unsafePairSBS
                                        _let2_aWln (toEncoding arg3_aWl2))
                                   <>
                                     (if omitField arg4_aWl3 then
                                          mempty
                                      else
                                          Data.Aeson.Encoding.Internal.unsafePairSBS
                                            _let3_aWlt (toEncoding arg4_aWl3)))))
instance FromJSON ResponseMessage where
  parseJSON
    = \ value_aWlx
        -> case value_aWlx of
             Object recObj_aWly
               -> ((((ResponseMessage
                        <$>
                          aesonLookupFieldOmit
                            omittedField parseJSON
                            "Language.LSP.Protocol.Message.Types.ResponseMessage"
                            "ResponseMessage" recObj_aWly
                            (Key.fromString "jsonrpc"))
                       <*>
                         aesonLookupFieldOmit
                           omittedField parseJSON
                           "Language.LSP.Protocol.Message.Types.ResponseMessage"
                           "ResponseMessage" recObj_aWly (Key.fromString "id"))
                      <*>
                        aesonLookupFieldOmit
                          omittedField parseJSON
                          "Language.LSP.Protocol.Message.Types.ResponseMessage"
                          "ResponseMessage" recObj_aWly (Key.fromString "result"))
                     <*>
                       aesonLookupFieldOmit
                         omittedField parseJSON
                         "Language.LSP.Protocol.Message.Types.ResponseMessage"
                         "ResponseMessage" recObj_aWly (Key.fromString "error"))
             other_aWm6
               -> aesonParseTypeMismatch'
                    "ResponseMessage"
                    "Language.LSP.Protocol.Message.Types.ResponseMessage" "Object"
                    (aesonValueConName other_aWm6)
deriving via ViaJSON ResponseMessage instance Pretty ResponseMessage

-----

-- | Typed notification message, containing the correct parameter payload.
data TNotificationMessage (m :: Method f Notification) = TNotificationMessage
  { _jsonrpc :: Text
  , _method :: SMethod m
  , _params :: MessageParams m
  }
  deriving stock (Generic)

deriving stock instance Eq (MessageParams m) => Eq (TNotificationMessage m)
deriving stock instance Show (MessageParams m) => Show (TNotificationMessage m)

{- Note [Missing 'params']
The 'params' field on requrests and notificaoins may be omitted according to the
JSON-RPC spec, but that doesn't quite work the way we want with the generic aeson
instance. Even if the 'MessageParams' type family happens to resolve to a 'Maybe',
we handle it generically and so we end up asserting that it must be present.

We fix this in a slightly dumb way by just adding the field in if it is missing,
set to null (which parses correctly for those 'Maybe' parameters also).
-}

instance (FromJSON (MessageParams m), FromJSON (SMethod m)) => FromJSON (TNotificationMessage m) where
  -- See Note [Missing 'params']
  parseJSON = genericParseJSON lspOptions . addNullField "params"
instance (ToJSON (MessageParams m)) => ToJSON (TNotificationMessage m) where
  toJSON = genericToJSON lspOptions
  toEncoding = genericToEncoding lspOptions

deriving via ViaJSON (TNotificationMessage m) instance (ToJSON (MessageParams m)) => Pretty (TNotificationMessage m)

-- | Typed request message, containing the correct parameter payload.
data TRequestMessage (m :: Method f Request) = TRequestMessage
  { _jsonrpc :: Text
  , _id :: LspId m
  , _method :: SMethod m
  , _params :: MessageParams m
  }
  deriving stock (Generic)

deriving stock instance Eq (MessageParams m) => Eq (TRequestMessage m)
deriving stock instance Show (MessageParams m) => Show (TRequestMessage m)

instance (FromJSON (MessageParams m), FromJSON (SMethod m)) => FromJSON (TRequestMessage m) where
  -- See Note [Missing 'params']
  parseJSON = genericParseJSON lspOptions . addNullField "params"
instance (ToJSON (MessageParams m)) => ToJSON (TRequestMessage m) where
  toJSON = genericToJSON lspOptions
  toEncoding = genericToEncoding lspOptions

deriving via ViaJSON (TRequestMessage m) instance (ToJSON (MessageParams m)) => Pretty (TRequestMessage m)

data TResponseError (m :: Method f Request) = TResponseError
  { _code :: LSPErrorCodes |? ErrorCodes
  , _message :: Text
  , _xdata :: Maybe (ErrorData m)
  }
  deriving stock (Generic)

deriving stock instance Eq (ErrorData m) => Eq (TResponseError m)
deriving stock instance Show (ErrorData m) => Show (TResponseError m)

instance (FromJSON (ErrorData m)) => FromJSON (TResponseError m) where
  parseJSON =
    let errorCode = withObject "ResponseError" $ \v ->
          TResponseError
            <$> v .: "code"
            <*> v .: "message"
            <*> v .:? "data"
     in fmap go . errorCode
   where
    go :: TResponseError m -> TResponseError m
    go x@(TResponseError (InL (LSPErrorCodes_Custom n)) _ _) =
      x{_code = InR (fromOpenEnumBaseType n)}
    go x = x
instance (ToJSON (ErrorData m)) => ToJSON (TResponseError m) where
  toJSON = genericToJSON lspOptions
  toEncoding = genericToEncoding lspOptions

deriving via ViaJSON (TResponseError m) instance (ToJSON (ErrorData m)) => Pretty (TResponseError m)

-- TODO: similar functions for the others?
toUntypedResponseError :: (ToJSON (ErrorData m)) => TResponseError m -> ResponseError
toUntypedResponseError (TResponseError c m d) = ResponseError c m (fmap toJSON d)

-- | A typed response message with a correct result payload.
data TResponseMessage (m :: Method f Request) = TResponseMessage
  { _jsonrpc :: Text
  , _id :: Maybe (LspId m)
  , -- TODO: use `TResponseError m` for the error type, this will require quite a lot of adaptation downstream
    _result :: Either ResponseError (MessageResult m)
  }
  deriving stock (Generic)

deriving stock instance (Eq (MessageResult m), Eq (ErrorData m)) => Eq (TResponseMessage m)
deriving stock instance (Show (MessageResult m), Show (ErrorData m)) => Show (TResponseMessage m)

instance (ToJSON (MessageResult m), ToJSON (ErrorData m)) => ToJSON (TResponseMessage m) where
  toJSON TResponseMessage{_jsonrpc = jsonrpc, _id = lspid, _result = result} =
    object
      [ "jsonrpc" .= jsonrpc
      , "id" .= lspid
      , case result of
          Left err -> "error" .= err
          Right a -> "result" .= a
      ]

instance (FromJSON (MessageResult a), FromJSON (ErrorData a)) => FromJSON (TResponseMessage a) where
  parseJSON = withObject "Response" $ \o -> do
    _jsonrpc <- o .: "jsonrpc"
    _id <- o .: "id"
    -- It is important to use .:! so that "result = null" (without error) gets decoded as Just Null
    _result <- o .:! "result"
    _error <- o .:? "error"
    result <- case (_error, _result) of
      (Just err, Nothing) -> pure $ Left err
      (Nothing, Just res) -> pure $ Right res
      (Just _err, Just _res) -> fail $ "both error and result cannot be present: " ++ show o
      (Nothing, Nothing) -> fail "both error and result cannot be Nothing"
    return $ TResponseMessage _jsonrpc _id result

deriving via ViaJSON (TResponseMessage m) instance (ToJSON (MessageResult m), ToJSON (ErrorData m)) => Pretty (TResponseMessage m)

{- | A typed custom message. A special data type is needed to distinguish between
 notifications and requests, since a CustomMethod can be both!
-}
data TCustomMessage s f t where
  ReqMess :: TRequestMessage (Method_CustomMethod s :: Method f Request) -> TCustomMessage s f Request
  NotMess :: TNotificationMessage (Method_CustomMethod s :: Method f Notification) -> TCustomMessage s f Notification

deriving stock instance Show (TCustomMessage s f t)

instance ToJSON (TCustomMessage s f t) where
  toJSON (ReqMess a) = toJSON a
  toJSON (NotMess a) = toJSON a

instance KnownSymbol s => FromJSON (TCustomMessage s f Request) where
  parseJSON v = ReqMess <$> parseJSON v
instance KnownSymbol s => FromJSON (TCustomMessage s f Notification) where
  parseJSON v = NotMess <$> parseJSON v

deriving via ViaJSON (TCustomMessage s f t) instance (KnownSymbol s) => Pretty (TCustomMessage s f t)

-- ---------------------------------------------------------------------
-- Helper Type Families
-- ---------------------------------------------------------------------

{- | Map a method to the Request/Notification type with the correct
 payload.
-}
type TMessage :: forall f t. Method f t -> Type
type family TMessage m where
  TMessage (Method_CustomMethod s :: Method f t) = TCustomMessage s f t
  TMessage (m :: Method f Request) = TRequestMessage m
  TMessage (m :: Method f Notification) = TNotificationMessage m

-- Some helpful type synonyms
type TClientMessage (m :: Method ClientToServer t) = TMessage m
type TServerMessage (m :: Method ServerToClient t) = TMessage m

{- | Replace a missing field in an object with a null field, to simplify parsing
 This is a hack to allow other types than Maybe to work like Maybe in allowing the field to be missing.
 See also this issue: https://github.com/haskell/aeson/issues/646
-}
addNullField :: String -> Value -> Value
addNullField s (Object o) = Object $ o <> fromString s .= J.Null
addNullField _ v = v
