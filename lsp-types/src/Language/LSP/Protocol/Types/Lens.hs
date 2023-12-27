{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Protocol.Types.Lens where

import qualified Control.Lens.Type
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokenModifiers
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokenTypes
import qualified Language.LSP.Protocol.Types.Common
import Language.LSP.Protocol.Internal.Lens
import Language.LSP.Protocol.Types.SemanticTokens


class HasLength s a | s -> a where
  length :: Control.Lens.Type.Lens' s a
instance HasLength SemanticTokenAbsolute Language.LSP.Protocol.Types.Common.UInt where
  {-# INLINE length #-}
  length
    f_a1sMN
    (SemanticTokenAbsolute x1_a1sMQ x2_a1sMR x3_a1sMS x4_a1sMT
                           x5_a1sMV)
    = fmap
        (\ y1_a1sMW
           -> SemanticTokenAbsolute
                x1_a1sMQ x2_a1sMR y1_a1sMW x4_a1sMT x5_a1sMV)
        (f_a1sMN x3_a1sMS)
instance HasLine SemanticTokenAbsolute Language.LSP.Protocol.Types.Common.UInt where
  {-# INLINE line #-}
  line
    f_a1sMX
    (SemanticTokenAbsolute x1_a1sMY x2_a1sMZ x3_a1sN0 x4_a1sN1
                           x5_a1sN2)
    = fmap
        (\ y1_a1sN3
           -> SemanticTokenAbsolute
                y1_a1sN3 x2_a1sMZ x3_a1sN0 x4_a1sN1 x5_a1sN2)
        (f_a1sMX x1_a1sMY)
class HasStartChar s a | s -> a where
  startChar :: Control.Lens.Type.Lens' s a
instance HasStartChar SemanticTokenAbsolute Language.LSP.Protocol.Types.Common.UInt where
  {-# INLINE startChar #-}
  startChar
    f_a1sN4
    (SemanticTokenAbsolute x1_a1sN5 x2_a1sN6 x3_a1sN7 x4_a1sN8
                           x5_a1sN9)
    = fmap
        (\ y1_a1sNa
           -> SemanticTokenAbsolute
                x1_a1sN5 y1_a1sNa x3_a1sN7 x4_a1sN8 x5_a1sN9)
        (f_a1sN4 x2_a1sN6)
instance HasTokenModifiers SemanticTokenAbsolute [Language.LSP.Protocol.Internal.Types.SemanticTokenModifiers.SemanticTokenModifiers] where
  {-# INLINE tokenModifiers #-}
  tokenModifiers
    f_a1sNi
    (SemanticTokenAbsolute x1_a1sNj x2_a1sNk x3_a1sNl x4_a1sNm
                           x5_a1sNn)
    = fmap
        (\ y1_a1sNo
           -> SemanticTokenAbsolute
                x1_a1sNj x2_a1sNk x3_a1sNl x4_a1sNm y1_a1sNo)
        (f_a1sNi x5_a1sNn)
class HasTokenType s a | s -> a where
  tokenType :: Control.Lens.Type.Lens' s a
instance HasTokenType SemanticTokenAbsolute Language.LSP.Protocol.Internal.Types.SemanticTokenTypes.SemanticTokenTypes where
  {-# INLINE tokenType #-}
  tokenType
    f_a1sNq
    (SemanticTokenAbsolute x1_a1sNr x2_a1sNs x3_a1sNt x4_a1sNu
                           x5_a1sNv)
    = fmap
        (\ y1_a1sNw
           -> SemanticTokenAbsolute
                x1_a1sNr x2_a1sNs x3_a1sNt y1_a1sNw x5_a1sNv)
        (f_a1sNq x4_a1sNu)

class HasDeltaLine s a | s -> a where
  deltaLine :: Control.Lens.Type.Lens' s a
instance HasDeltaLine SemanticTokenRelative Language.LSP.Protocol.Types.Common.UInt where
  {-# INLINE deltaLine #-}
  deltaLine
    f_a1sVL
    (SemanticTokenRelative x1_a1sVM x2_a1sVN x3_a1sVO x4_a1sVP
                           x5_a1sVQ)
    = fmap
        (\ y1_a1sVR
           -> SemanticTokenRelative
                y1_a1sVR x2_a1sVN x3_a1sVO x4_a1sVP x5_a1sVQ)
        (f_a1sVL x1_a1sVM)
class HasDeltaStartChar s a | s -> a where
  deltaStartChar :: Control.Lens.Type.Lens' s a
instance HasDeltaStartChar SemanticTokenRelative Language.LSP.Protocol.Types.Common.UInt where
  {-# INLINE deltaStartChar #-}
  deltaStartChar
    f_a1sVU
    (SemanticTokenRelative x1_a1sVV x2_a1sVW x3_a1sVX x4_a1sVY
                           x5_a1sVZ)
    = fmap
        (\ y1_a1sW0
           -> SemanticTokenRelative
                x1_a1sVV y1_a1sW0 x3_a1sVX x4_a1sVY x5_a1sVZ)
        (f_a1sVU x2_a1sVW)
instance HasLength SemanticTokenRelative Language.LSP.Protocol.Types.Common.UInt where
  {-# INLINE length #-}
  length
    f_a1sW1
    (SemanticTokenRelative x1_a1sW2 x2_a1sW3 x3_a1sW4 x4_a1sW5
                           x5_a1sW6)
    = fmap
        (\ y1_a1sW7
           -> SemanticTokenRelative
                x1_a1sW2 x2_a1sW3 y1_a1sW7 x4_a1sW5 x5_a1sW6)
        (f_a1sW1 x3_a1sW4)
instance HasTokenModifiers SemanticTokenRelative [Language.LSP.Protocol.Internal.Types.SemanticTokenModifiers.SemanticTokenModifiers] where
  {-# INLINE tokenModifiers #-}
  tokenModifiers
    f_a1sWa
    (SemanticTokenRelative x1_a1sWb x2_a1sWc x3_a1sWd x4_a1sWe
                           x5_a1sWf)
    = fmap
        (\ y1_a1sWg
           -> SemanticTokenRelative
                x1_a1sWb x2_a1sWc x3_a1sWd x4_a1sWe y1_a1sWg)
        (f_a1sWa x5_a1sWf)
instance HasTokenType SemanticTokenRelative Language.LSP.Protocol.Internal.Types.SemanticTokenTypes.SemanticTokenTypes where
  {-# INLINE tokenType #-}
  tokenType
    f_a1sWl
    (SemanticTokenRelative x1_a1sWm x2_a1sWn x3_a1sWo x4_a1sWp
                           x5_a1sWq)
    = fmap
        (\ y1_a1sWr
           -> SemanticTokenRelative
                x1_a1sWm x2_a1sWn x3_a1sWo y1_a1sWr x5_a1sWq)
        (f_a1sWl x4_a1sWp)