{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Protocol.Message.Lens where

-- import Control.Lens.TH
import qualified Control.Lens.Type
import qualified Data.Text.Internal
import qualified GHC.Int
import qualified Language.LSP.Protocol.Internal.Method
import qualified Language.LSP.Protocol.Internal.Types.ErrorCodes
import qualified Language.LSP.Protocol.Internal.Types.LSPErrorCodes
import qualified Language.LSP.Protocol.Message.LspId
import qualified Language.LSP.Protocol.Message.Meta
import qualified Language.LSP.Protocol.Message.Method
import qualified Language.LSP.Protocol.Types.Common

import Data.Aeson.Types
import Language.LSP.Protocol.Internal.Lens
import Language.LSP.Protocol.Message.Registration
import Language.LSP.Protocol.Message.Types

instance HasId (TRegistration (m_iRUT :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iRUS)) Data.Text.Internal.Text where
  {-# INLINE id #-}
  id f_a112t (TRegistration x1_a112u x2_a112v x3_a112w)
    = fmap
        (\ y1_a112x -> TRegistration y1_a112x x2_a112v x3_a112w)
        (f_a112t x1_a112u)
instance HasMethod (TRegistration (m_iRUT :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iRUS)) (Language.LSP.Protocol.Message.Method.SClientMethod m_iRUT) where
  {-# INLINE method #-}
  method f_a118w (TRegistration x1_a118x x2_a118y x3_a118z)
    = fmap
        (\ y1_a118A -> TRegistration x1_a118x y1_a118A x3_a118z)
        (f_a118w x2_a118y)
instance (a_a118T
          ~
          Maybe (Language.LSP.Protocol.Internal.Method.RegistrationOptions m_iRUT)) =>
         HasRegisterOptions (TRegistration (m_iRUT :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iRUS)) a_a118T where
  {-# INLINE registerOptions #-}
  registerOptions f_a118U (TRegistration x1_a118V x2_a118W x3_a118X)
    = fmap
        (\ y1_a118Y -> TRegistration x1_a118V x2_a118W y1_a118Y)
        (f_a118U x3_a118X)

instance HasId (TUnregistration (m_iRVc :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iRVb)) Data.Text.Internal.Text where
  {-# INLINE id #-}
  id f_a11f1 (TUnregistration x1_a11f2 x2_a11f3)
    = fmap
        (\ y1_a11f4 -> TUnregistration y1_a11f4 x2_a11f3)
        (f_a11f1 x1_a11f2)
instance HasMethod (TUnregistration (m_iRVc :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iRVb)) (Language.LSP.Protocol.Internal.Method.SMethod m_iRVc) where
  {-# INLINE method #-}
  method f_a11fd (TUnregistration x1_a11fe x2_a11ff)
    = fmap
        (\ y1_a11fg -> TUnregistration x1_a11fe y1_a11fg)
        (f_a11fd x2_a11ff)

instance HasId RequestMessage ((Language.LSP.Protocol.Types.Common.|?) GHC.Int.Int32 Data.Text.Internal.Text) where
  {-# INLINE id #-}
  id f_a11hd (RequestMessage x1_a11he x2_a11hf x3_a11hg x4_a11hh)
    = fmap
        (\ y1_a11hi -> RequestMessage x1_a11he y1_a11hi x3_a11hg x4_a11hh)
        (f_a11hd x2_a11hf)
class HasJsonrpc s a | s -> a where
  jsonrpc :: Control.Lens.Type.Lens' s a
instance HasJsonrpc RequestMessage Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc
    f_a11hj
    (RequestMessage x1_a11hk x2_a11hl x3_a11hm x4_a11hn)
    = fmap
        (\ y1_a11ho -> RequestMessage y1_a11ho x2_a11hl x3_a11hm x4_a11hn)
        (f_a11hj x1_a11hk)
instance HasMethod RequestMessage Data.Text.Internal.Text where
  {-# INLINE method #-}
  method f_a11hp (RequestMessage x1_a11hq x2_a11hr x3_a11hs x4_a11ht)
    = fmap
        (\ y1_a11hu -> RequestMessage x1_a11hq x2_a11hr y1_a11hu x4_a11ht)
        (f_a11hp x3_a11hs)
class HasParams s a | s -> a where
  params :: Control.Lens.Type.Lens' s a
instance HasParams RequestMessage (Maybe Data.Aeson.Types.Value) where
  {-# INLINE params #-}
  params f_a11hz (RequestMessage x1_a11hA x2_a11hB x3_a11hC x4_a11hD)
    = fmap
        (\ y1_a11hE -> RequestMessage x1_a11hA x2_a11hB x3_a11hC y1_a11hE)
        (f_a11hz x4_a11hD)

class HasError s a | s -> a where
  error :: Control.Lens.Type.Lens' s a
instance HasError ResponseMessage (Maybe ResponseError) where
  {-# INLINE error #-}
  error f_a11lI (ResponseMessage x1_a11lJ x2_a11lK x3_a11lL x4_a11lM)
    = fmap
        (\ y1_a11lN -> ResponseMessage x1_a11lJ x2_a11lK x3_a11lL y1_a11lN)
        (f_a11lI x4_a11lM)
instance HasId ResponseMessage ((Language.LSP.Protocol.Types.Common.|?) GHC.Int.Int32 ((Language.LSP.Protocol.Types.Common.|?) Data.Text.Internal.Text Language.LSP.Protocol.Types.Common.Null)) where
  {-# INLINE id #-}
  id f_a11m4 (ResponseMessage x1_a11m5 x2_a11m6 x3_a11m7 x4_a11m8)
    = fmap
        (\ y1_a11m9 -> ResponseMessage x1_a11m5 y1_a11m9 x3_a11m7 x4_a11m8)
        (f_a11m4 x2_a11m6)
instance HasJsonrpc ResponseMessage Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc
    f_a11ma
    (ResponseMessage x1_a11mb x2_a11mc x3_a11md x4_a11me)
    = fmap
        (\ y1_a11mf -> ResponseMessage y1_a11mf x2_a11mc x3_a11md x4_a11me)
        (f_a11ma x1_a11mb)
class HasResult s a | s -> a where
  result :: Control.Lens.Type.Lens' s a
instance HasResult ResponseMessage (Maybe Data.Aeson.Types.Value) where
  {-# INLINE result #-}
  result
    f_a11mk
    (ResponseMessage x1_a11ml x2_a11mm x3_a11mn x4_a11mo)
    = fmap
        (\ y1_a11mp -> ResponseMessage x1_a11ml x2_a11mm y1_a11mp x4_a11mo)
        (f_a11mk x3_a11mn)

instance HasJsonrpc NotificationMessage Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc f_a11rP (NotificationMessage x1_a11rQ x2_a11rR x3_a11rS)
    = fmap
        (\ y1_a11rT -> NotificationMessage y1_a11rT x2_a11rR x3_a11rS)
        (f_a11rP x1_a11rQ)
instance HasMethod NotificationMessage Data.Text.Internal.Text where
  {-# INLINE method #-}
  method f_a11rU (NotificationMessage x1_a11rV x2_a11rW x3_a11rX)
    = fmap
        (\ y1_a11rY -> NotificationMessage x1_a11rV y1_a11rY x3_a11rX)
        (f_a11rU x2_a11rW)
instance HasParams NotificationMessage (Maybe Data.Aeson.Types.Value) where
  {-# INLINE params #-}
  params f_a11s3 (NotificationMessage x1_a11s4 x2_a11s5 x3_a11s6)
    = fmap
        (\ y1_a11s7 -> NotificationMessage x1_a11s4 x2_a11s5 y1_a11s7)
        (f_a11s3 x3_a11s6)

instance HasCode ResponseError ((Language.LSP.Protocol.Types.Common.|?) Language.LSP.Protocol.Internal.Types.LSPErrorCodes.LSPErrorCodes Language.LSP.Protocol.Internal.Types.ErrorCodes.ErrorCodes) where
  {-# INLINE code #-}
  code f_a11u0 (ResponseError x1_a11u1 x2_a11u2 x3_a11u3)
    = fmap
        (\ y1_a11u4 -> ResponseError y1_a11u4 x2_a11u2 x3_a11u3)
        (f_a11u0 x1_a11u1)
instance HasMessage ResponseError Data.Text.Internal.Text where
  {-# INLINE message #-}
  message f_a11u5 (ResponseError x1_a11u6 x2_a11u7 x3_a11u8)
    = fmap
        (\ y1_a11u9 -> ResponseError x1_a11u6 y1_a11u9 x3_a11u8)
        (f_a11u5 x2_a11u7)
class HasXdata s a | s -> a where
  xdata :: Control.Lens.Type.Lens' s a
instance HasXdata ResponseError (Maybe Data.Aeson.Types.Value) where
  {-# INLINE xdata #-}
  xdata f_a11ue (ResponseError x1_a11uf x2_a11ug x3_a11uh)
    = fmap
        (\ y1_a11ui -> ResponseError x1_a11uf x2_a11ug y1_a11ui)
        (f_a11ue x3_a11uh)

instance HasId (TRequestMessage (m_iRVw :: Language.LSP.Protocol.Internal.Method.Method f_iRVv 'Language.LSP.Protocol.Message.Meta.Request)) (Language.LSP.Protocol.Message.LspId.LspId m_iRVw) where
  {-# INLINE id #-}
  id f_a11wP (TRequestMessage x1_a11wQ x2_a11wR x3_a11wS x4_a11wT)
    = fmap
        (\ y1_a11wU -> TRequestMessage x1_a11wQ y1_a11wU x3_a11wS x4_a11wT)
        (f_a11wP x2_a11wR)
instance HasJsonrpc (TRequestMessage (m_iRVw :: Language.LSP.Protocol.Internal.Method.Method f_iRVv 'Language.LSP.Protocol.Message.Meta.Request)) Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc
    f_a11wV
    (TRequestMessage x1_a11wW x2_a11wX x3_a11wY x4_a11wZ)
    = fmap
        (\ y1_a11x0 -> TRequestMessage y1_a11x0 x2_a11wX x3_a11wY x4_a11wZ)
        (f_a11wV x1_a11wW)
instance HasMethod (TRequestMessage (m_iRVw :: Language.LSP.Protocol.Internal.Method.Method f_iRVv 'Language.LSP.Protocol.Message.Meta.Request)) (Language.LSP.Protocol.Internal.Method.SMethod m_iRVw) where
  {-# INLINE method #-}
  method
    f_a11x9
    (TRequestMessage x1_a11xa x2_a11xb x3_a11xc x4_a11xd)
    = fmap
        (\ y1_a11xe -> TRequestMessage x1_a11xa x2_a11xb y1_a11xe x4_a11xd)
        (f_a11x9 x3_a11xc)
instance (a_a11xh
          ~ Language.LSP.Protocol.Internal.Method.MessageParams m_iRVw) =>
         HasParams (TRequestMessage (m_iRVw :: Language.LSP.Protocol.Internal.Method.Method f_iRVv 'Language.LSP.Protocol.Message.Meta.Request)) a_a11xh where
  {-# INLINE params #-}
  params
    f_a11xi
    (TRequestMessage x1_a11xj x2_a11xk x3_a11xl x4_a11xm)
    = fmap
        (\ y1_a11xn -> TRequestMessage x1_a11xj x2_a11xk x3_a11xl y1_a11xn)
        (f_a11xi x4_a11xm)

instance HasId (TResponseMessage (m_iRY1 :: Language.LSP.Protocol.Internal.Method.Method f_iRY0 'Language.LSP.Protocol.Message.Meta.Request)) (Maybe (Language.LSP.Protocol.Message.LspId.LspId m_iRY1)) where
  {-# INLINE id #-}
  id f_a11BG (TResponseMessage x1_a11BH x2_a11BI x3_a11BJ)
    = fmap
        (\ y1_a11BK -> TResponseMessage x1_a11BH y1_a11BK x3_a11BJ)
        (f_a11BG x2_a11BI)
instance HasJsonrpc (TResponseMessage (m_iRY1 :: Language.LSP.Protocol.Internal.Method.Method f_iRY0 'Language.LSP.Protocol.Message.Meta.Request)) Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc f_a11BL (TResponseMessage x1_a11BM x2_a11BN x3_a11BO)
    = fmap
        (\ y1_a11BP -> TResponseMessage y1_a11BP x2_a11BN x3_a11BO)
        (f_a11BL x1_a11BM)
instance (a_a11C1
          ~
          Either ResponseError (Language.LSP.Protocol.Internal.Method.MessageResult m_iRY1)) =>
         HasResult (TResponseMessage (m_iRY1 :: Language.LSP.Protocol.Internal.Method.Method f_iRY0 'Language.LSP.Protocol.Message.Meta.Request)) a_a11C1 where
  {-# INLINE result #-}
  result f_a11C2 (TResponseMessage x1_a11C3 x2_a11C4 x3_a11C5)
    = fmap
        (\ y1_a11C6 -> TResponseMessage x1_a11C3 x2_a11C4 y1_a11C6)
        (f_a11C2 x3_a11C5)

instance HasJsonrpc (TNotificationMessage (m_iRVn :: Language.LSP.Protocol.Internal.Method.Method f_iRVm 'Language.LSP.Protocol.Message.Meta.Notification)) Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc f_a11Ia (TNotificationMessage x1_a11Ib x2_a11Ic x3_a11Id)
    = fmap
        (\ y1_a11Ie -> TNotificationMessage y1_a11Ie x2_a11Ic x3_a11Id)
        (f_a11Ia x1_a11Ib)
instance HasMethod (TNotificationMessage (m_iRVn :: Language.LSP.Protocol.Internal.Method.Method f_iRVm 'Language.LSP.Protocol.Message.Meta.Notification)) (Language.LSP.Protocol.Internal.Method.SMethod m_iRVn) where
  {-# INLINE method #-}
  method f_a11IT (TNotificationMessage x1_a11IU x2_a11IV x3_a11IW)
    = fmap
        (\ y1_a11IX -> TNotificationMessage x1_a11IU y1_a11IX x3_a11IW)
        (f_a11IT x2_a11IV)
instance (a_a11Jh
          ~ Language.LSP.Protocol.Internal.Method.MessageParams m_iRVn) =>
         HasParams (TNotificationMessage (m_iRVn :: Language.LSP.Protocol.Internal.Method.Method f_iRVm 'Language.LSP.Protocol.Message.Meta.Notification)) a_a11Jh where
  {-# INLINE params #-}
  params f_a11Jp (TNotificationMessage x1_a11Js x2_a11Ju x3_a11Jw)
    = fmap
        (\ y1_a11Jx -> TNotificationMessage x1_a11Js x2_a11Ju y1_a11Jx)
        (f_a11Jp x3_a11Jw)

instance HasCode (TResponseError (m_i11Tf :: Language.LSP.Protocol.Internal.Method.Method f_i11Te 'Language.LSP.Protocol.Message.Meta.Request)) ((Language.LSP.Protocol.Types.Common.|?) Language.LSP.Protocol.Internal.Types.LSPErrorCodes.LSPErrorCodes Language.LSP.Protocol.Internal.Types.ErrorCodes.ErrorCodes) where
  {-# INLINE code #-}
  code f_a11TY (TResponseError x1_a11TZ x2_a11U0 x3_a11U1)
    = fmap
        (\ y1_a11U2 -> TResponseError y1_a11U2 x2_a11U0 x3_a11U1)
        (f_a11TY x1_a11TZ)
instance HasMessage (TResponseError (m_i11Tf :: Language.LSP.Protocol.Internal.Method.Method f_i11Te 'Language.LSP.Protocol.Message.Meta.Request)) Data.Text.Internal.Text where
  {-# INLINE message #-}
  message f_a11Uf (TResponseError x1_a11Ug x2_a11Uh x3_a11Ui)
    = fmap
        (\ y1_a11Uj -> TResponseError x1_a11Ug y1_a11Uj x3_a11Ui)
        (f_a11Uf x2_a11Uh)
instance (a_a11UN
          ~
          Maybe (Language.LSP.Protocol.Internal.Method.ErrorData m_i11Tf)) =>
         HasXdata (TResponseError (m_i11Tf :: Language.LSP.Protocol.Internal.Method.Method f_i11Te 'Language.LSP.Protocol.Message.Meta.Request)) a_a11UN where
  {-# INLINE xdata #-}
  xdata f_a11UO (TResponseError x1_a11UP x2_a11UQ x3_a11UR)
    = fmap
        (\ y1_a11US -> TResponseError x1_a11UP x2_a11UQ y1_a11US)
        (f_a11UO x3_a11UR)
