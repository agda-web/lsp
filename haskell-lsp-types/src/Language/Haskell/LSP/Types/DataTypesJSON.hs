{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Language.Haskell.LSP.Types.DataTypesJSON where

import qualified Data.Aeson                                 as A
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Bits                                  (testBit)
import           Data.Scientific                            (floatingOrInteger)
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Language.Haskell.LSP.Types.ClientCapabilities
import           Language.Haskell.LSP.Types.Common
import           Language.Haskell.LSP.Types.LspId
import           Language.Haskell.LSP.Types.Method
import           Language.Haskell.LSP.Types.Progress
import           Language.Haskell.LSP.Types.ServerCapabilities
import           Language.Haskell.LSP.Types.Uri
import           Language.Haskell.LSP.Types.Utils
import           Language.Haskell.LSP.Types.WorkspaceEdit
import           Language.Haskell.LSP.Types.WorkspaceFolders

-- =====================================================================
-- ACTUAL PROTOCOL -----------------------------------------------------
-- =====================================================================

-- ---------------------------------------------------------------------
-- Initialize Request
-- ---------------------------------------------------------------------
{-
Initialize Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#initialize-request

The initialize request is sent as the first request from the client to the server.

Request

    method: 'initialize'
    params: InitializeParams defined as follows:

interface InitializeParams {
        /**
         * The process Id of the parent process that started
         * the server. Is null if the process has not been started by another process.
         * If the parent process is not alive then the server should exit (see exit notification) its process.
         */
        processId: number | null;

        /**
         * The rootPath of the workspace. Is null
         * if no folder is open.
         *
         * @deprecated in favour of rootUri.
         */
        rootPath?: string | null;

        /**
         * The rootUri of the workspace. Is null if no
         * folder is open. If both `rootPath` and `rootUri` are set
         * `rootUri` wins.
         */
        rootUri: DocumentUri | null;

        /**
         * User provided initialization options.
         */
        initializationOptions?: any;

        /**
         * The capabilities provided by the client (editor or tool)
         */
        capabilities: ClientCapabilities;

        /**
         * The initial trace setting. If omitted trace is disabled ('off').
         */
        trace?: 'off' | 'messages' | 'verbose';
}
-}

data Trace = TraceOff | TraceMessages | TraceVerbose
           deriving (Show, Read, Eq)

instance A.ToJSON Trace where
  toJSON TraceOff      = A.String (T.pack "off")
  toJSON TraceMessages = A.String (T.pack "messages")
  toJSON TraceVerbose  = A.String (T.pack "verbose")

instance A.FromJSON Trace where
  parseJSON (A.String s) = case T.unpack s of
    "off"      -> return TraceOff
    "messages" -> return TraceMessages
    "verbose"  -> return TraceVerbose
    _          -> mempty
  parseJSON _                               = mempty

data InitializeParams =
  InitializeParams {
    _processId             :: Maybe Int
  , _rootPath              :: Maybe Text -- ^ Deprecated in favour of _rootUri
  , _rootUri               :: Maybe Uri
  , _initializationOptions :: Maybe A.Value
  , _capabilities          :: ClientCapabilities
  , _trace                 :: Maybe Trace
  -- |  The workspace folders configured in the client when the server starts.
  -- This property is only available if the client supports workspace folders.
  -- It can be `null` if the client supports workspace folders but none are
  -- configured.
  -- Since LSP 3.6
  --
  -- @since 0.7.0.0
  , _workspaceFolders      :: Maybe (List WorkspaceFolder)
  , _workDoneToken         :: Maybe ProgressToken
  } deriving (Show, Read, Eq)

{-# DEPRECATED _rootPath "Use _rootUri" #-}

deriveJSON lspOptions ''InitializeParams

-- ---------------------------------------------------------------------
-- Initialize Response
-- ---------------------------------------------------------------------
{-

    error.data:

interface InitializeError {
    /**
     * Indicates whether the client should retry to send the
     * initilize request after showing the message provided
     * in the ResponseError.
     */
    retry: boolean;
-
-}
data InitializeError =
  InitializeError
    { _retry :: Bool
    } deriving (Read, Show, Eq)

deriveJSON lspOptions ''InitializeError

-- ---------------------------------------------------------------------
-- |
--   Information about the capabilities of a language server
--
data InitializeResponseCapabilities =
  InitializeResponseCapabilities {
    _capabilities :: ServerCapabilities
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''InitializeResponseCapabilities

-- ---------------------------------------------------------------------

{-
    error.code:

/**
 * Known error codes for an `InitializeError`;
 */
export namespace InitializeError {
        /**
         * If the protocol version provided by the client can't be handled by the server.
         * @deprecated This initialize error got replaced by client capabilities. There is
         * no version handshake in version 3.0x
         */
        export const unknownProtocolVersion: number = 1;
}

    error.data:

interface InitializeError {
        /**
         * Indicates whether the client execute the following retry logic:
         * (1) show the message provided by the ResponseError to the user
         * (2) user selects retry or cancel
         * (3) if user selected retry the initialize method is sent again.
         */
        retry: boolean;
}
-}

-- ---------------------------------------------------------------------

{-
New in 3.0
----------
Initialized Notification

The initialized notification is sent from the client to the server after the
client is fully initialized and is able to listen to arbritary requests and
notifications sent from the server.

Notification:

    method: 'initialized'
    params: void

-}

data InitializedParams =
  InitializedParams
    {
    } deriving (Show, Read, Eq)

instance A.FromJSON InitializedParams where
  parseJSON (A.Object _) = pure InitializedParams
  parseJSON _            = mempty

instance A.ToJSON InitializedParams where
  toJSON InitializedParams = A.Object mempty

-- ---------------------------------------------------------------------
{-
Shutdown Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#shutdown-request

The shutdown request is sent from the client to the server. It asks the server
to shut down, but to not exit (otherwise the response might not be delivered
correctly to the client). There is a separate exit notification that asks the
server to exit.

Request

    method: 'shutdown'
    params: undefined

Response

    result: undefined
    error: code and message set in case an exception happens during shutdown request.


-}

-- ---------------------------------------------------------------------
{-
Exit Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#exit-notification

A notification to ask the server to exit its process.

Notification

    method: 'exit'

-}

-- |
--   Notification from the server to actually exit now, after shutdown acked
--
data ExitParams =
  ExitParams
    {
    } deriving (Show, Read, Eq)

deriveJSON defaultOptions ''ExitParams

-- ---------------------------------------------------------------------
{-
Telemetry Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#telemetry-notification

    New: The telemetry notification is sent from the server to the client to ask
    the client to log a telemetry event.

Notification:

    method: 'telemetry/event'
    params: 'any'
-}

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

Unregister Capability

The client/unregisterCapability request is sent from the server to the client to
unregister a previously register capability.

Request:

    method: 'client/unregisterCapability'
    params: UnregistrationParams

Where UnregistrationParams are defined as follows:

/**
 * General parameters to unregister a capability.
 */
export interface Unregistration {
        /**
         * The id used to unregister the request or notification. Usually an id
         * provided during the register request.
         */
        id: string;

        /**
         * The method / capability to unregister for.
         */
        method: string;
}

export interface UnregistrationParams {
        unregisterations: Unregistration[];
}
-}

data Unregistration =
  Unregistration
    { -- | The id used to unregister the request or notification. Usually an id
      -- provided during the register request.
      _id     :: Text

       -- | The method / capability to unregister for.
    , _method :: SomeClientMethod
    } deriving (Show, Eq)

deriveJSON lspOptions ''Unregistration

data UnregistrationParams =
  UnregistrationParams
    { _unregistrations :: List Unregistration
    } deriving (Show, Eq)

deriveJSON lspOptions ''UnregistrationParams

-- ---------------------------------------------------------------------

-- /**
--  * Describe options to be used when registering for file system change events.
--  */
-- export interface DidChangeWatchedFilesRegistrationOptions {
-- 	/**
-- 	 * The watchers to register.
-- 	 */
-- 	watchers: FileSystemWatcher[];
-- }
--
-- export interface FileSystemWatcher {
-- 	/**
-- 	 * The  glob pattern to watch.
-- 	 *
-- 	 * Glob patterns can have the following syntax:
-- 	 * - `*` to match one or more characters in a path segment
-- 	 * - `?` to match on one character in a path segment
-- 	 * - `**` to match any number of path segments, including none
-- 	 * - `{}` to group conditions (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
-- 	 * - `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
-- 	 * - `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)
-- 	 */
-- 	globPattern: string;
--
-- 	/**
-- 	 * The kind of events of interest. If omitted it defaults
-- 	 * to WatchKind.Create | WatchKind.Change | WatchKind.Delete
-- 	 * which is 7.
-- 	 */
-- 	kind?: number;
-- }
--
-- export namespace WatchKind {
-- 	/**
-- 	 * Interested in create events.
-- 	 */
-- 	export const Create = 1;
--
-- 	/**
-- 	 * Interested in change events
-- 	 */
-- 	export const Change = 2;
--
-- 	/**
-- 	 * Interested in delete events
-- 	 */
-- 	export const Delete = 4;
-- }

data DidChangeWatchedFilesRegistrationOptions =
  DidChangeWatchedFilesRegistrationOptions {
    _watchers :: List FileSystemWatcher
  } deriving (Show, Read, Eq)

data FileSystemWatcher =
  FileSystemWatcher {
    _globPattern :: String,
    _kind :: Maybe WatchKind
  } deriving (Show, Read, Eq)

data WatchKind =
  WatchKind {
    -- | Watch for create events
    _watchCreate :: Bool,
    -- | Watch for change events
    _watchChange :: Bool,
    -- | Watch for delete events
    _watchDelete :: Bool
  } deriving (Show, Read, Eq)

instance A.ToJSON WatchKind where
  toJSON wk = A.Number (createNum + changeNum + deleteNum)
    where
      createNum = if _watchCreate wk then 0x1 else 0x0
      changeNum = if _watchChange wk then 0x2 else 0x0
      deleteNum = if _watchDelete wk then 0x4 else 0x0

instance A.FromJSON WatchKind where
  parseJSON (A.Number n)
    | Right i <- floatingOrInteger n :: Either Double Int
    , 0 <= i && i <= 7 =
        pure $ WatchKind (testBit i 0x0) (testBit i 0x1) (testBit i 0x2)
    | otherwise = mempty
  parseJSON _            = mempty

deriveJSON lspOptions ''FileSystemWatcher
deriveJSON lspOptions ''DidChangeWatchedFilesRegistrationOptions

-- ---------------------------------------------------------------------
{-
DidChangeConfiguration Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#didchangeconfiguration-notification

A notification sent from the client to the server to signal the change of
configuration settings.

Notification:

    method: 'workspace/didChangeConfiguration',
    params: DidChangeConfigurationParams defined as follows:

interface DidChangeConfigurationParams {
    /**
     * The actual changed settings
     */
    settings: any;
}
-}

data DidChangeConfigurationParams =
  DidChangeConfigurationParams {
    _settings :: A.Value
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DidChangeConfigurationParams

-- ---------------------------------------------------------------------

{-
Configuration Request (:arrow_right_hook:)
Since version 3.6.0

The workspace/configuration request is sent from the server to the client to
fetch configuration settings from the client. The request can fetch n
configuration settings in one roundtrip. The order of the returned configuration
settings correspond to the order of the passed ConfigurationItems (e.g. the
first item in the response is the result for the first configuration item in the
params).

A ConfigurationItem consist of the configuration section to ask for and an
additional scope URI. The configuration section ask for is defined by the server
and doesn’t necessarily need to correspond to the configuration store used be
the client. So a server might ask for a configuration cpp.formatterOptions but
the client stores the configuration in a XML store layout differently. It is up
to the client to do the necessary conversion. If a scope URI is provided the
client should return the setting scoped to the provided resource. If the client
for example uses EditorConfig to manage its settings the configuration should be
returned for the passed resource URI. If the client can’t provide a
configuration setting for a given scope then null need to be present in the
returned array.

Request:

method: ‘workspace/configuration’
params: ConfigurationParams defined as follows
export interface ConfigurationParams {
        items: ConfigurationItem[];
}

export interface ConfigurationItem {
        /**
         * The scope to get the configuration section for.
         */
        scopeUri?: string;

        /**
         * The configuration section asked for.
         */
        section?: string;
}
Response:

result: any[]
error: code and message set in case an exception happens during the
‘workspace/configuration’ request
-}

data ConfigurationItem =
  ConfigurationItem
    { _scopeUri :: Maybe Text -- ^ The scope to get the configuration section for.
    , _section  :: Maybe Text -- ^ The configuration section asked for.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ConfigurationItem

data ConfigurationParams =
  ConfigurationParams
    { _items :: List ConfigurationItem
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ConfigurationParams

-- ---------------------------------------------------------------------
{-
DidChangeWatchedFiles Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#didchangewatchedfiles-notification

The watched files notification is sent from the client to the server when the
client detects changes to files watched by the language client.

Notification:

    method: 'workspace/didChangeWatchedFiles'
    params: DidChangeWatchedFilesParams defined as follows:

interface DidChangeWatchedFilesParams {
    /**
     * The actual file events.
     */
    changes: FileEvent[];
}

Where FileEvents are described as follows:

/**
 * The file event type.
 */
enum FileChangeType {
    /**
     * The file got created.
     */
    Created = 1,
    /**
     * The file got changed.
     */
    Changed = 2,
    /**
     * The file got deleted.
     */
    Deleted = 3
}

/**
 * An event describing a file change.
 */
interface FileEvent {
    /**
     * The file's URI.
     */
    uri: string;
    /**
     * The change type.
     */
    type: number;
-}
data FileChangeType = FcCreated
                    | FcChanged
                    | FcDeleted
       deriving (Read,Show,Eq)

instance A.ToJSON FileChangeType where
  toJSON FcCreated = A.Number 1
  toJSON FcChanged = A.Number 2
  toJSON FcDeleted = A.Number 3

instance A.FromJSON FileChangeType where
  parseJSON (A.Number 1) = pure FcCreated
  parseJSON (A.Number 2) = pure FcChanged
  parseJSON (A.Number 3) = pure FcDeleted
  parseJSON _            = mempty


-- -------------------------------------

data FileEvent =
  FileEvent
    { _uri   :: Uri
    , _xtype :: FileChangeType
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''FileEvent

data DidChangeWatchedFilesParams =
  DidChangeWatchedFilesParams
    { _changes :: List FileEvent
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DidChangeWatchedFilesParams

-- ---------------------------------------------------------------------
{-
PublishDiagnostics Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#publishdiagnostics-notification

Diagnostics notification are sent from the server to the client to signal
results of validation runs.

Notification

    method: 'textDocument/publishDiagnostics'
    params: PublishDiagnosticsParams defined as follows:

interface PublishDiagnosticsParams {
    /**
     * The URI for which diagnostic information is reported.
     */
    uri: string;

    /**
     * An array of diagnostic information items.
     */
    diagnostics: Diagnostic[];
}
-}


-- ---------------------------------------------------------------------
{-
Goto Definition Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#goto-definition-request

The go to definition request is sent from the client to the server to resolve the definition location of a symbol at a given text document position.

The result type LocationLink[] got introduced with version 3.14.0 and depends on the corresponding client capability textDocument.definition.linkSupport.

Client Capability:

property name (optional): textDocument.definition
property type: DefinitionClientCapabilities defined as follows:
export interface DefinitionClientCapabilities {
	/**
	 * Whether definition supports dynamic registration.
	 */
	dynamicRegistration?: boolean;

	/**
	 * The client supports additional metadata in the form of definition links.
	 *
	 * @since 3.14.0
	 */
	linkSupport?: boolean;
}
Server Capability:

property name (optional): definitionProvider
property type: boolean | DefinitionOptions where DefinitionOptions is defined as follows:
export interface DefinitionOptions extends WorkDoneProgressOptions {
}
Registration Options: DefinitionRegistrationOptions defined as follows:

export interface DefinitionRegistrationOptions extends TextDocumentRegistrationOptions, DefinitionOptions {
}
Request:

method: ‘textDocument/definition’
params: DefinitionParams defined as follows:
export interface DefinitionParams extends TextDocumentPositionParams, WorkDoneProgressParams, PartialResultParams {
}
Response:

result: Location | Location[] | LocationLink[] | null
partial result: Location[] | LocationLink[]
error: code and message set in case an exception happens during the definition request.

-}

-- {"jsonrpc":"2.0","id":1,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///tmp/Foo.hs"},"position":{"line":1,"character":8}}}

-- ---------------------------------------------------------------------
{-
Workspace Symbols Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#workspace-symbols-request

The workspace symbol request is sent from the client to the server to list
project-wide symbols matching the query string.

Request

    method: 'workspace/symbol'
    params: WorkspaceSymbolParams defined as follows:

/**
 * The parameters of a Workspace Symbol Request.
 */
interface WorkspaceSymbolParams {
    /**
     * A non-empty query string
     */
    query: string;
}

Response

    result: SymbolInformation[] as defined above.
    error: code and message set in case an exception happens during the
           workspace symbol request.
-}


makeExtendingDatatype "WorkspaceSymbolParams" [''WorkDoneProgressParams, ''PartialResultParams]
  [("_query", [t| String |])]

deriveJSON lspOptions ''WorkspaceSymbolParams

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

Execute a command

The workspace/executeCommand request is sent from the client to the server to
trigger command execution on the server. In most cases the server creates a
WorkspaceEdit structure and applies the changes to the workspace using the
request workspace/applyEdit which is sent from the server to the client.

Request:

    method: 'workspace/executeCommand'
    params: ExecuteCommandParams defined as follows:

export interface ExecuteCommandParams {

        /**
         * The identifier of the actual command handler.
         */
        command: string;
        /**
         * Arguments that the command should be invoked with.
         */
        arguments?: any[];
}

The arguments are typically specified when a command is returned from the server
to the client. Example requests that return a command are
textDocument/codeAction or textDocument/codeLens.

Response:

    result: any
    error: code and message set in case an exception happens during the request.

Registration Options: ExecuteCommandRegistrationOptions defined as follows:

/**
 * Execute command registration options.
 */
export interface ExecuteCommandRegistrationOptions {
        /**
         * The commands to be executed on the server
         */
        commands: string[]
}
-}

data ExecuteCommandParams =
  ExecuteCommandParams
    { _command   :: Text -- ^ The identifier of the actual command handler.
    , _arguments :: Maybe (List A.Value) -- ^ Arguments that the command should be invoked with.
    , _workDoneToken :: Maybe ProgressToken -- ^ An optional token that a server can use to report work done progress.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ExecuteCommandParams

data ExecuteCommandRegistrationOptions =
  ExecuteCommandRegistrationOptions
    { _commands :: List Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ExecuteCommandRegistrationOptions

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

Applies a WorkspaceEdit

The workspace/applyEdit request is sent from the server to the client to modify
resource on the client side.

Request:

    method: 'workspace/applyEdit'
    params: ApplyWorkspaceEditParams defined as follows:

export interface ApplyWorkspaceEditParams {
        /**
         * The edits to apply.
         */
        edit: WorkspaceEdit;
}

Response:

    result: ApplyWorkspaceEditResponse defined as follows:

export interface ApplyWorkspaceEditResponse {
        /**
         * Indicates whether the edit was applied or not.
         */
        applied: boolean;
}

    error: code and message set in case an exception happens during the request.

-}
data ApplyWorkspaceEditParams =
  ApplyWorkspaceEditParams
    { _edit :: WorkspaceEdit
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ApplyWorkspaceEditParams

data ApplyWorkspaceEditResponseBody =
  ApplyWorkspaceEditResponseBody
    { _applied :: Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ApplyWorkspaceEditResponseBody

-- ---------------------------------------------------------------------

data TraceParams =
  TraceParams {
    _value :: Text
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TraceParams


data TraceNotification =
  TraceNotification {
    _params :: TraceParams
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TraceNotification

-- ---------------------------------------------------------------------
{-
Cancellation Support

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#cancellation-support

    New: The base protocol now offers support for request cancellation. To
    cancel a request, a notification message with the following properties is
    sent:

Notification:

    method: '$/cancelRequest'
    params: CancelParams defined as follows:

interface CancelParams {
    /**
     * The request id to cancel.
     */
    id: number | string;
}

A request that got canceled still needs to return from the server and send a
response back. It can not be left open / hanging. This is in line with the JSON
RPC protocol that requires that every request sends a response back. In addition
it allows for returning partial results on cancel.
-}

data CancelParams = forall m.
  CancelParams
    { _id :: LspId m
    }

deriving instance Read CancelParams
deriving instance Show CancelParams
instance Eq CancelParams where
  (CancelParams a) == CancelParams b =
    case (a,b) of
      (IdInt x, IdInt y) -> x == y
      (IdString x, IdString y) -> x == y
      _ -> False

deriveJSON lspOptions ''CancelParams

-- ---------------------------------------------------------------------
