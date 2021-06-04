{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}

--------------------------------------------------------------------------------

#include <Yoga.h>
#include <bindings.dsl.h>

--------------------------------------------------------------------------------

module Bindings.Yoga.Enums where

import Prelude (Num)
import Foreign.C.Types  (CUInt(..))

#integral_t enum YGAlign
#num YGAlignAuto
#num YGAlignFlexStart
#num YGAlignCenter
#num YGAlignFlexEnd
#num YGAlignStretch
#num YGAlignBaseline
#num YGAlignSpaceBetween
#num YGAlignSpaceAround

#integral_t enum YGDimension
#num YGDimensionWidth
#num YGDimensionHeight

#integral_t enum YGDirection
#num YGDirectionInherit
#num YGDirectionLTR
#num YGDirectionRTL

#integral_t enum YGDisplay
#num YGDisplayFlex
#num YGDisplayNone

#integral_t enum YGEdge
#num YGEdgeLeft
#num YGEdgeTop
#num YGEdgeRight
#num YGEdgeBottom
#num YGEdgeStart
#num YGEdgeEnd
#num YGEdgeHorizontal
#num YGEdgeVertical
#num YGEdgeAll

#integral_t enum YGExperimentalFeature
#num YGExperimentalFeatureWebFlexBasis

#integral_t enum YGFlexDirection
#num YGFlexDirectionColumn
#num YGFlexDirectionColumnReverse
#num YGFlexDirectionRow
#num YGFlexDirectionRowReverse

#integral_t enum YGJustify
#num YGJustifyFlexStart
#num YGJustifyCenter
#num YGJustifyFlexEnd
#num YGJustifySpaceBetween
#num YGJustifySpaceAround
#num YGJustifySpaceEvenly

#integral_t enum YGLogLevel
#num YGLogLevelError
#num YGLogLevelWarn
#num YGLogLevelInfo
#num YGLogLevelDebug
#num YGLogLevelVerbose
#num YGLogLevelFatal

#integral_t enum YGMeasureMode
#num YGMeasureModeUndefined
#num YGMeasureModeExactly
#num YGMeasureModeAtMost

#integral_t enum YGNodeType
#num YGNodeTypeDefault
#num YGNodeTypeText

#integral_t enum YGOverflow
#num YGOverflowVisible
#num YGOverflowHidden
#num YGOverflowScroll

#integral_t enum YGPositionType
#num YGPositionTypeStatic
#num YGPositionTypeRelative
#num YGPositionTypeAbsolute

#integral_t enum YGPrintOptions
#num YGPrintOptionsLayout
#num YGPrintOptionsStyle
#num YGPrintOptionsChildren

#integral_t enum YGUnit
#num YGUnitUndefined
#num YGUnitPoint
#num YGUnitPercent
#num YGUnitAuto

#integral_t enum YGWrap
#num YGWrapNoWrap
#num YGWrapWrap
#num YGWrapWrapReverse
