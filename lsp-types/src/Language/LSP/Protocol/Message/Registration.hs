{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}

module Language.LSP.Protocol.Message.Registration where

import Language.LSP.Protocol.Internal.Method
import Language.LSP.Protocol.Message.Meta
import Language.LSP.Protocol.Message.Method
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Utils.Misc

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Prettyprinter

-- | Typed registration type, with correct options.
data TRegistration (m :: Method ClientToServer t) = TRegistration
  { _id :: Text
  -- ^ The id used to register the request. The id can be used to deregister
  -- the request again.
  , _method :: SClientMethod m
  -- ^ The method / capability to register for.
  , _registerOptions :: !(Maybe (RegistrationOptions m))
  -- ^ Options necessary for the registration.
  -- Make this strict to aid the pattern matching exhaustiveness checker
  }
  deriving stock (Generic)

deriving stock instance Eq (RegistrationOptions m) => Eq (TRegistration m)
deriving stock instance Show (RegistrationOptions m) => Show (TRegistration m)

-- TODO: can we do this generically somehow?
-- This generates the function
-- regHelper :: SMethod m
--           -> (( Show (RegistrationOptions m)
--               , ToJSON (RegistrationOptions m)
--               , FromJSON ($regOptTcon m)
--              => x)
--           -> x

regHelper :: SMethod m
  -> (Show (RegistrationOptions m) =>
      ToJSON (RegistrationOptions m) =>
      FromJSON (RegistrationOptions m) => x)
     -> x

regHelper SMethod_TextDocumentImplementation x_aUER = x_aUER
regHelper SMethod_TextDocumentTypeDefinition x_aUES = x_aUES
regHelper SMethod_TextDocumentDocumentColor x_aUET = x_aUET
regHelper SMethod_TextDocumentColorPresentation x_aUEU = x_aUEU
regHelper SMethod_TextDocumentFoldingRange x_aUEV = x_aUEV
regHelper SMethod_TextDocumentDeclaration x_aUEW = x_aUEW
regHelper SMethod_TextDocumentSelectionRange x_aUEX = x_aUEX
regHelper SMethod_TextDocumentPrepareCallHierarchy x_aUEY = x_aUEY
regHelper SMethod_CallHierarchyIncomingCalls x_aUEZ = x_aUEZ
regHelper SMethod_CallHierarchyOutgoingCalls x_aUF0 = x_aUF0
regHelper SMethod_TextDocumentSemanticTokensFull x_aUF1 = x_aUF1
regHelper SMethod_TextDocumentSemanticTokensFullDelta x_aUF2
  = x_aUF2
regHelper SMethod_TextDocumentSemanticTokensRange x_aUF3 = x_aUF3
regHelper SMethod_TextDocumentLinkedEditingRange x_aUF4 = x_aUF4
regHelper SMethod_WorkspaceWillCreateFiles x_aUF5 = x_aUF5
regHelper SMethod_WorkspaceWillRenameFiles x_aUF6 = x_aUF6
regHelper SMethod_WorkspaceWillDeleteFiles x_aUF7 = x_aUF7
regHelper SMethod_TextDocumentMoniker x_aUF8 = x_aUF8
regHelper SMethod_TextDocumentPrepareTypeHierarchy x_aUF9 = x_aUF9
regHelper SMethod_TypeHierarchySupertypes x_aUFa = x_aUFa
regHelper SMethod_TypeHierarchySubtypes x_aUFb = x_aUFb
regHelper SMethod_TextDocumentInlineValue x_aUFc = x_aUFc
regHelper SMethod_TextDocumentInlayHint x_aUFd = x_aUFd
regHelper SMethod_InlayHintResolve x_aUFe = x_aUFe
regHelper SMethod_TextDocumentDiagnostic x_aUFf = x_aUFf
regHelper SMethod_WorkspaceDiagnostic x_aUFg = x_aUFg
regHelper SMethod_Initialize x_aUFh = x_aUFh
regHelper SMethod_Shutdown x_aUFi = x_aUFi
regHelper SMethod_TextDocumentWillSaveWaitUntil x_aUFj = x_aUFj
regHelper SMethod_TextDocumentCompletion x_aUFk = x_aUFk
regHelper SMethod_CompletionItemResolve x_aUFl = x_aUFl
regHelper SMethod_TextDocumentHover x_aUFm = x_aUFm
regHelper SMethod_TextDocumentSignatureHelp x_aUFn = x_aUFn
regHelper SMethod_TextDocumentDefinition x_aUFo = x_aUFo
regHelper SMethod_TextDocumentReferences x_aUFp = x_aUFp
regHelper SMethod_TextDocumentDocumentHighlight x_aUFq = x_aUFq
regHelper SMethod_TextDocumentDocumentSymbol x_aUFr = x_aUFr
regHelper SMethod_TextDocumentCodeAction x_aUFs = x_aUFs
regHelper SMethod_CodeActionResolve x_aUFt = x_aUFt
regHelper SMethod_WorkspaceSymbol x_aUFu = x_aUFu
regHelper SMethod_WorkspaceSymbolResolve x_aUFv = x_aUFv
regHelper SMethod_TextDocumentCodeLens x_aUFw = x_aUFw
regHelper SMethod_CodeLensResolve x_aUFx = x_aUFx
regHelper SMethod_TextDocumentDocumentLink x_aUFy = x_aUFy
regHelper SMethod_DocumentLinkResolve x_aUFz = x_aUFz
regHelper SMethod_TextDocumentFormatting x_aUFA = x_aUFA
regHelper SMethod_TextDocumentRangeFormatting x_aUFB = x_aUFB
regHelper SMethod_TextDocumentOnTypeFormatting x_aUFC = x_aUFC
regHelper SMethod_TextDocumentRename x_aUFD = x_aUFD
regHelper SMethod_TextDocumentPrepareRename x_aUFE = x_aUFE
regHelper SMethod_WorkspaceExecuteCommand x_aUFF = x_aUFF
regHelper SMethod_WorkspaceDidChangeWorkspaceFolders x_aUFG
  = x_aUFG
regHelper SMethod_WindowWorkDoneProgressCancel x_aUFH = x_aUFH
regHelper SMethod_WorkspaceDidCreateFiles x_aUFI = x_aUFI
regHelper SMethod_WorkspaceDidRenameFiles x_aUFJ = x_aUFJ
regHelper SMethod_WorkspaceDidDeleteFiles x_aUFK = x_aUFK
regHelper SMethod_NotebookDocumentDidOpen x_aUFL = x_aUFL
regHelper SMethod_NotebookDocumentDidChange x_aUFM = x_aUFM
regHelper SMethod_NotebookDocumentDidSave x_aUFN = x_aUFN
regHelper SMethod_NotebookDocumentDidClose x_aUFO = x_aUFO
regHelper SMethod_Initialized x_aUFP = x_aUFP
regHelper SMethod_Exit x_aUFQ = x_aUFQ
regHelper SMethod_WorkspaceDidChangeConfiguration x_aUFR = x_aUFR
regHelper SMethod_TextDocumentDidOpen x_aUFS = x_aUFS
regHelper SMethod_TextDocumentDidChange x_aUFT = x_aUFT
regHelper SMethod_TextDocumentDidClose x_aUFU = x_aUFU
regHelper SMethod_TextDocumentDidSave x_aUFV = x_aUFV
regHelper SMethod_TextDocumentWillSave x_aUFW = x_aUFW
regHelper SMethod_WorkspaceDidChangeWatchedFiles x_aUFX = x_aUFX
regHelper SMethod_SetTrace x_aUFY = x_aUFY

instance ToJSON (TRegistration m) where
  toJSON x@(TRegistration _ m _) = regHelper m (genericToJSON lspOptions x)

deriving via ViaJSON (TRegistration m) instance Pretty (TRegistration m)

data SomeRegistration = forall t (m :: Method ClientToServer t). SomeRegistration (TRegistration m)

instance ToJSON SomeRegistration where
  toJSON (SomeRegistration r) = toJSON r

instance FromJSON SomeRegistration where
  parseJSON = withObject "Registration" $ \o -> do
    SomeClientMethod m <- o .: "method"
    r <- TRegistration <$> o .: "id" <*> pure m <*> regHelper m (o .: "registerOptions")
    pure (SomeRegistration r)

instance Show SomeRegistration where
  show (SomeRegistration r@(TRegistration _ m _)) = regHelper m (show r)

deriving via ViaJSON SomeRegistration instance Pretty SomeRegistration

toUntypedRegistration :: TRegistration m -> Registration
toUntypedRegistration (TRegistration i meth opts) = Registration i (T.pack $ someMethodToMethodString $ SomeMethod meth) (Just $ regHelper meth (toJSON opts))

toSomeRegistration :: Registration -> Maybe SomeRegistration
toSomeRegistration r =
  let v = toJSON r
   in case fromJSON v of
        Success r' -> Just r'
        _ -> Nothing

-- ---------------------------------------------------------------------

-- | Typed unregistration type.
data TUnregistration (m :: Method ClientToServer t) = TUnregistration
  { _id :: Text
  -- ^ The id used to unregister the request or notification. Usually an id
  -- provided during the register request.
  , _method :: SMethod m
  -- ^ The method / capability to unregister for.
  }
  deriving stock (Generic)

deriving stock instance Eq (TUnregistration m)
deriving stock instance Show (TUnregistration m)

instance ToJSON (TUnregistration m) where
  toJSON x@(TUnregistration _ m) = regHelper m (genericToJSON lspOptions x)

deriving via ViaJSON (TUnregistration m) instance Pretty (TUnregistration m)

data SomeUnregistration = forall t (m :: Method ClientToServer t). SomeUnregistration (TUnregistration m)

instance ToJSON SomeUnregistration where
  toJSON (SomeUnregistration r) = toJSON r

instance FromJSON SomeUnregistration where
  parseJSON = withObject "Unregistration" $ \o -> do
    SomeClientMethod m <- o .: "method"
    r <- TUnregistration <$> o .: "id" <*> pure m
    pure (SomeUnregistration r)

deriving via ViaJSON SomeUnregistration instance Pretty SomeUnregistration

toUntypedUnregistration :: TUnregistration m -> Unregistration
toUntypedUnregistration (TUnregistration i meth) = Unregistration i (T.pack $ someMethodToMethodString $ SomeMethod meth)

toSomeUnregistration :: Unregistration -> Maybe SomeUnregistration
toSomeUnregistration r =
  let v = toJSON r
   in case fromJSON v of
        Success r' -> Just r'
        _ -> Nothing
