{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Protocol.Message.Lens where

import Control.Lens.TH
import Language.LSP.Protocol.Internal.Lens
import Language.LSP.Protocol.Message.Registration
import Language.LSP.Protocol.Message.Types

makeFieldsNoPrefix ''TRegistration
makeFieldsNoPrefix ''TUnregistration
makeFieldsNoPrefix ''RequestMessage
makeFieldsNoPrefix ''ResponseMessage
makeFieldsNoPrefix ''NotificationMessage
makeFieldsNoPrefix ''ResponseError
makeFieldsNoPrefix ''TRequestMessage
makeFieldsNoPrefix ''TResponseMessage
makeFieldsNoPrefix ''TNotificationMessage
makeFieldsNoPrefix ''TResponseError
