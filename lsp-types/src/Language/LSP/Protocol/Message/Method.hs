{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Protocol.Message.Method where

import Data.Aeson.Types
import Data.Function (on)
import Data.GADT.Compare
import Data.List (isPrefixOf)
import Data.Proxy
import Data.Type.Equality
import GHC.Exts (Int (..), dataToTag#)
import GHC.TypeLits (
  KnownSymbol,
  sameSymbol,
  symbolVal,
 )
import Language.LSP.Protocol.Internal.Method
import Language.LSP.Protocol.Message.Meta
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import Unsafe.Coerce

import Control.Monad

---------------
-- SomeMethod
---------------

-- | Is this an "optional" method which servers and clients are allowed to ignore?
isOptionalMethod :: SomeMethod -> Bool
-- See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#dollarRequests
isOptionalMethod m = "$/" `isPrefixOf` someMethodToMethodString m

deriving stock instance Show SomeMethod
instance Eq SomeMethod where
  (==) = (==) `on` someMethodToMethodString
instance Ord SomeMethod where
  compare = compare `on` someMethodToMethodString

instance ToJSON SomeMethod where
  toJSON sm = toJSON $ someMethodToMethodString sm

instance FromJSON SomeMethod where
  parseJSON v = do
    s <- parseJSON v
    pure $ methodStringToSomeMethod s

deriving via ViaJSON SomeMethod instance Pretty SomeMethod

---------------
-- SMethod
---------------

-- This instance is written manually rather than derived to avoid a dependency
-- on 'dependent-sum-template'.
instance GEq SMethod where
  geq x y = case gcompare x y of
    GLT -> Nothing
    GEQ -> Just Refl
    GGT -> Nothing

-- This instance is written manually rather than derived to avoid a dependency
-- on 'dependent-sum-template'.
instance GCompare SMethod where
  gcompare (SMethod_CustomMethod x) (SMethod_CustomMethod y) = case symbolVal x `compare` symbolVal y of
    LT -> GLT
    EQ -> unsafeCoerce GEQ
    GT -> GGT
  -- This is much more compact than matching on every pair of constructors, which
  -- is what we would need to do for GHC to 'see' that this is correct. Nonetheless
  -- it is safe: since there is only one constructor of 'SMethod' for each 'Method',
  -- the argument types can only be equal if the constructor tag is equal.
  gcompare x y = case I# (dataToTag# x) `compare` I# (dataToTag# y) of
    LT -> GLT
    EQ -> unsafeCoerce GEQ
    GT -> GGT

instance Eq (SMethod m) where
  -- This defers to 'GEq', ensuring that this version is compatible.
  (==) = defaultEq

instance Ord (SMethod m) where
  -- This defers to 'GCompare', ensuring that this version is compatible.
  compare = defaultCompare

deriving stock instance Show (SMethod m)

instance ToJSON (SMethod m) where
  toJSON m = toJSON (SomeMethod m)

instance KnownSymbol s => FromJSON (SMethod ('Method_CustomMethod s :: Method f t)) where
  parseJSON v = do
    sm <- parseJSON v
    case sm of
      SomeMethod (SMethod_CustomMethod x) -> case sameSymbol x (Proxy :: Proxy s) of
        Just Refl -> pure $ SMethod_CustomMethod x
        Nothing -> mempty
      _ -> mempty

-- TODO: generate these with everything else?
-- Generates lots of instances like this in terms of the FromJSON SomeMethod instance
-- instance FromJSON (SMethod Method_X)
instance FromJSON (SMethod 'Method_TextDocumentImplementation) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentImplementation
                -> pure SMethod_TextDocumentImplementation
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentTypeDefinition) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentTypeDefinition
                -> pure SMethod_TextDocumentTypeDefinition
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceWorkspaceFolders) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceWorkspaceFolders
                -> pure SMethod_WorkspaceWorkspaceFolders
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceConfiguration) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceConfiguration
                -> pure SMethod_WorkspaceConfiguration
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentDocumentColor) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentDocumentColor
                -> pure SMethod_TextDocumentDocumentColor
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentColorPresentation) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentColorPresentation
                -> pure SMethod_TextDocumentColorPresentation
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentFoldingRange) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentFoldingRange
                -> pure SMethod_TextDocumentFoldingRange
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentDeclaration) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentDeclaration
                -> pure SMethod_TextDocumentDeclaration
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentSelectionRange) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentSelectionRange
                -> pure SMethod_TextDocumentSelectionRange
              _ -> mempty))
instance FromJSON (SMethod 'Method_WindowWorkDoneProgressCreate) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WindowWorkDoneProgressCreate
                -> pure SMethod_WindowWorkDoneProgressCreate
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentPrepareCallHierarchy) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentPrepareCallHierarchy
                -> pure SMethod_TextDocumentPrepareCallHierarchy
              _ -> mempty))
instance FromJSON (SMethod 'Method_CallHierarchyIncomingCalls) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_CallHierarchyIncomingCalls
                -> pure SMethod_CallHierarchyIncomingCalls
              _ -> mempty))
instance FromJSON (SMethod 'Method_CallHierarchyOutgoingCalls) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_CallHierarchyOutgoingCalls
                -> pure SMethod_CallHierarchyOutgoingCalls
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentSemanticTokensFull) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentSemanticTokensFull
                -> pure SMethod_TextDocumentSemanticTokensFull
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentSemanticTokensFullDelta) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentSemanticTokensFullDelta
                -> pure SMethod_TextDocumentSemanticTokensFullDelta
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentSemanticTokensRange) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentSemanticTokensRange
                -> pure SMethod_TextDocumentSemanticTokensRange
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceSemanticTokensRefresh) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceSemanticTokensRefresh
                -> pure SMethod_WorkspaceSemanticTokensRefresh
              _ -> mempty))
instance FromJSON (SMethod 'Method_WindowShowDocument) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WindowShowDocument
                -> pure SMethod_WindowShowDocument
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentLinkedEditingRange) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentLinkedEditingRange
                -> pure SMethod_TextDocumentLinkedEditingRange
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceWillCreateFiles) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceWillCreateFiles
                -> pure SMethod_WorkspaceWillCreateFiles
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceWillRenameFiles) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceWillRenameFiles
                -> pure SMethod_WorkspaceWillRenameFiles
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceWillDeleteFiles) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceWillDeleteFiles
                -> pure SMethod_WorkspaceWillDeleteFiles
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentMoniker) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentMoniker
                -> pure SMethod_TextDocumentMoniker
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentPrepareTypeHierarchy) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentPrepareTypeHierarchy
                -> pure SMethod_TextDocumentPrepareTypeHierarchy
              _ -> mempty))
instance FromJSON (SMethod 'Method_TypeHierarchySupertypes) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TypeHierarchySupertypes
                -> pure SMethod_TypeHierarchySupertypes
              _ -> mempty))
instance FromJSON (SMethod 'Method_TypeHierarchySubtypes) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TypeHierarchySubtypes
                -> pure SMethod_TypeHierarchySubtypes
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentInlineValue) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentInlineValue
                -> pure SMethod_TextDocumentInlineValue
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceInlineValueRefresh) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceInlineValueRefresh
                -> pure SMethod_WorkspaceInlineValueRefresh
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentInlayHint) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentInlayHint
                -> pure SMethod_TextDocumentInlayHint
              _ -> mempty))
instance FromJSON (SMethod 'Method_InlayHintResolve) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_InlayHintResolve
                -> pure SMethod_InlayHintResolve
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceInlayHintRefresh) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceInlayHintRefresh
                -> pure SMethod_WorkspaceInlayHintRefresh
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentDiagnostic) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentDiagnostic
                -> pure SMethod_TextDocumentDiagnostic
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceDiagnostic) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceDiagnostic
                -> pure SMethod_WorkspaceDiagnostic
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceDiagnosticRefresh) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceDiagnosticRefresh
                -> pure SMethod_WorkspaceDiagnosticRefresh
              _ -> mempty))
instance FromJSON (SMethod 'Method_ClientRegisterCapability) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_ClientRegisterCapability
                -> pure SMethod_ClientRegisterCapability
              _ -> mempty))
instance FromJSON (SMethod 'Method_ClientUnregisterCapability) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_ClientUnregisterCapability
                -> pure SMethod_ClientUnregisterCapability
              _ -> mempty))
instance FromJSON (SMethod 'Method_Initialize) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_Initialize -> pure SMethod_Initialize
              _ -> mempty))
instance FromJSON (SMethod 'Method_Shutdown) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_Shutdown -> pure SMethod_Shutdown
              _ -> mempty))
instance FromJSON (SMethod 'Method_WindowShowMessageRequest) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WindowShowMessageRequest
                -> pure SMethod_WindowShowMessageRequest
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentWillSaveWaitUntil) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentWillSaveWaitUntil
                -> pure SMethod_TextDocumentWillSaveWaitUntil
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentCompletion) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentCompletion
                -> pure SMethod_TextDocumentCompletion
              _ -> mempty))
instance FromJSON (SMethod 'Method_CompletionItemResolve) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_CompletionItemResolve
                -> pure SMethod_CompletionItemResolve
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentHover) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentHover
                -> pure SMethod_TextDocumentHover
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentSignatureHelp) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentSignatureHelp
                -> pure SMethod_TextDocumentSignatureHelp
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentDefinition) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentDefinition
                -> pure SMethod_TextDocumentDefinition
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentReferences) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentReferences
                -> pure SMethod_TextDocumentReferences
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentDocumentHighlight) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentDocumentHighlight
                -> pure SMethod_TextDocumentDocumentHighlight
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentDocumentSymbol) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentDocumentSymbol
                -> pure SMethod_TextDocumentDocumentSymbol
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentCodeAction) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentCodeAction
                -> pure SMethod_TextDocumentCodeAction
              _ -> mempty))
instance FromJSON (SMethod 'Method_CodeActionResolve) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_CodeActionResolve
                -> pure SMethod_CodeActionResolve
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceSymbol) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceSymbol -> pure SMethod_WorkspaceSymbol
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceSymbolResolve) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceSymbolResolve
                -> pure SMethod_WorkspaceSymbolResolve
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentCodeLens) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentCodeLens
                -> pure SMethod_TextDocumentCodeLens
              _ -> mempty))
instance FromJSON (SMethod 'Method_CodeLensResolve) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_CodeLensResolve -> pure SMethod_CodeLensResolve
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceCodeLensRefresh) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceCodeLensRefresh
                -> pure SMethod_WorkspaceCodeLensRefresh
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentDocumentLink) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentDocumentLink
                -> pure SMethod_TextDocumentDocumentLink
              _ -> mempty))
instance FromJSON (SMethod 'Method_DocumentLinkResolve) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_DocumentLinkResolve
                -> pure SMethod_DocumentLinkResolve
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentFormatting) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentFormatting
                -> pure SMethod_TextDocumentFormatting
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentRangeFormatting) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentRangeFormatting
                -> pure SMethod_TextDocumentRangeFormatting
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentOnTypeFormatting) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentOnTypeFormatting
                -> pure SMethod_TextDocumentOnTypeFormatting
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentRename) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentRename
                -> pure SMethod_TextDocumentRename
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentPrepareRename) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentPrepareRename
                -> pure SMethod_TextDocumentPrepareRename
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceExecuteCommand) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceExecuteCommand
                -> pure SMethod_WorkspaceExecuteCommand
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceApplyEdit) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceApplyEdit
                -> pure SMethod_WorkspaceApplyEdit
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceDidChangeWorkspaceFolders) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceDidChangeWorkspaceFolders
                -> pure SMethod_WorkspaceDidChangeWorkspaceFolders
              _ -> mempty))
instance FromJSON (SMethod 'Method_WindowWorkDoneProgressCancel) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WindowWorkDoneProgressCancel
                -> pure SMethod_WindowWorkDoneProgressCancel
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceDidCreateFiles) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceDidCreateFiles
                -> pure SMethod_WorkspaceDidCreateFiles
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceDidRenameFiles) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceDidRenameFiles
                -> pure SMethod_WorkspaceDidRenameFiles
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceDidDeleteFiles) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceDidDeleteFiles
                -> pure SMethod_WorkspaceDidDeleteFiles
              _ -> mempty))
instance FromJSON (SMethod 'Method_NotebookDocumentDidOpen) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_NotebookDocumentDidOpen
                -> pure SMethod_NotebookDocumentDidOpen
              _ -> mempty))
instance FromJSON (SMethod 'Method_NotebookDocumentDidChange) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_NotebookDocumentDidChange
                -> pure SMethod_NotebookDocumentDidChange
              _ -> mempty))
instance FromJSON (SMethod 'Method_NotebookDocumentDidSave) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_NotebookDocumentDidSave
                -> pure SMethod_NotebookDocumentDidSave
              _ -> mempty))
instance FromJSON (SMethod 'Method_NotebookDocumentDidClose) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_NotebookDocumentDidClose
                -> pure SMethod_NotebookDocumentDidClose
              _ -> mempty))
instance FromJSON (SMethod 'Method_Initialized) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_Initialized -> pure SMethod_Initialized
              _ -> mempty))
instance FromJSON (SMethod 'Method_Exit) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_Exit -> pure SMethod_Exit
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceDidChangeConfiguration) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceDidChangeConfiguration
                -> pure SMethod_WorkspaceDidChangeConfiguration
              _ -> mempty))
instance FromJSON (SMethod 'Method_WindowShowMessage) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WindowShowMessage
                -> pure SMethod_WindowShowMessage
              _ -> mempty))
instance FromJSON (SMethod 'Method_WindowLogMessage) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WindowLogMessage
                -> pure SMethod_WindowLogMessage
              _ -> mempty))
instance FromJSON (SMethod 'Method_TelemetryEvent) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TelemetryEvent -> pure SMethod_TelemetryEvent
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentDidOpen) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentDidOpen
                -> pure SMethod_TextDocumentDidOpen
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentDidChange) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentDidChange
                -> pure SMethod_TextDocumentDidChange
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentDidClose) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentDidClose
                -> pure SMethod_TextDocumentDidClose
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentDidSave) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentDidSave
                -> pure SMethod_TextDocumentDidSave
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentWillSave) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentWillSave
                -> pure SMethod_TextDocumentWillSave
              _ -> mempty))
instance FromJSON (SMethod 'Method_WorkspaceDidChangeWatchedFiles) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_WorkspaceDidChangeWatchedFiles
                -> pure SMethod_WorkspaceDidChangeWatchedFiles
              _ -> mempty))
instance FromJSON (SMethod 'Method_TextDocumentPublishDiagnostics) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_TextDocumentPublishDiagnostics
                -> pure SMethod_TextDocumentPublishDiagnostics
              _ -> mempty))
instance FromJSON (SMethod 'Method_SetTrace) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_SetTrace -> pure SMethod_SetTrace
              _ -> mempty))
instance FromJSON (SMethod 'Method_LogTrace) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_LogTrace -> pure SMethod_LogTrace
              _ -> mempty))
instance FromJSON (SMethod ('Method_CancelRequest :: Method f_ixTsL 'Notification)) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_CancelRequest -> pure SMethod_CancelRequest
              _ -> mempty))
instance FromJSON (SMethod ('Method_Progress :: Method f_ixTsL 'Notification)) where
  parseJSON
    = (parseJSON
          Control.Monad.>=>
            (\case
              SomeMethod SMethod_Progress -> pure SMethod_Progress
              _ -> mempty))


deriving via ViaJSON (SMethod m) instance Pretty (SMethod m)

---------------
-- Extras
---------------

-- Some useful type synonyms
type SClientMethod (m :: Method ClientToServer t) = SMethod m
type SServerMethod (m :: Method ServerToClient t) = SMethod m

data SomeClientMethod = forall t (m :: Method ClientToServer t). SomeClientMethod (SMethod m)
deriving stock instance Show SomeClientMethod

data SomeServerMethod = forall t (m :: Method ServerToClient t). SomeServerMethod (SMethod m)
deriving stock instance Show SomeServerMethod

someClientMethod :: SMethod m -> Maybe SomeClientMethod
someClientMethod s = case messageDirection s of
  SClientToServer -> Just $ SomeClientMethod s
  SServerToClient -> Nothing
  -- See Note [Parsing methods that go both ways]
  SBothDirections -> Just $ SomeClientMethod $ unsafeCoerce s

someServerMethod :: SMethod m -> Maybe SomeServerMethod
someServerMethod s = case messageDirection s of
  SServerToClient -> Just $ SomeServerMethod s
  SClientToServer -> Nothing
  -- See Note [Parsing methods that go both ways]
  SBothDirections -> Just $ SomeServerMethod $ unsafeCoerce s

instance FromJSON SomeClientMethod where
  parseJSON v = do
    (SomeMethod sm) <- parseJSON v
    case someClientMethod sm of
      Just scm -> pure scm
      Nothing -> mempty

instance ToJSON SomeClientMethod where
  toJSON (SomeClientMethod sm) = toJSON $ someMethodToMethodString $ SomeMethod sm

deriving via ViaJSON SomeClientMethod instance Pretty SomeClientMethod

instance FromJSON SomeServerMethod where
  parseJSON v = do
    (SomeMethod sm) <- parseJSON v
    case someServerMethod sm of
      Just scm -> pure scm
      Nothing -> mempty

instance ToJSON SomeServerMethod where
  toJSON (SomeServerMethod sm) = toJSON $ someMethodToMethodString $ SomeMethod sm

deriving via ViaJSON SomeServerMethod instance Pretty SomeServerMethod

{- Note [Parsing methods that go both ways]

In order to parse a method as a client or server method, we use 'messageDirection'
to get a proof that the message direction is what we say it is. But this just doesn't
work for the both directions case: because we are awkwardly representing "both directions"
as "the type variable is free". So the types don't line up and we use an awful hack.

A better solution might be to move away from using an unconstrained type parameter to
mean "both directions".
-}
