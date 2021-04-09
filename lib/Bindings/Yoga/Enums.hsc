{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}

--------------------------------------------------------------------------------

#include <Yoga.h>
#include <bindings.dsl.h>

--------------------------------------------------------------------------------

module Bindings.Yoga.Enums where

import Prelude (Num)

-- typedef enum YGFlexDirection
#num YGFlexDirectionColumn
#num YGFlexDirectionColumnReverse
#num YGFlexDirectionRow
#num YGFlexDirectionRowReverse

-- typedef enum YGMeasureMode {
#num YGMeasureModeUndefined
#num YGMeasureModeExactly
#num YGMeasureModeAtMost

-- typedef enum YGPrintOptions {
#num YGPrintOptionsLayout
#num YGPrintOptionsStyle
#num YGPrintOptionsChildren

-- typedef enum YGEdge {
#num YGEdgeLeft
#num YGEdgeTop
#num YGEdgeRight
#num YGEdgeBottom
#num YGEdgeStart
#num YGEdgeEnd
#num YGEdgeHorizontal
#num YGEdgeVertical
#num YGEdgeAll

-- typedef enum YGPositionType {
#num YGPositionTypeRelative
#num YGPositionTypeAbsolute

-- typedef enum YGDimension {
#num YGDimensionWidth
#num YGDimensionHeight

-- typedef enum YGJustify {
#num YGJustifyFlexStart
#num YGJustifyCenter
#num YGJustifyFlexEnd
#num YGJustifySpaceBetween
#num YGJustifySpaceAround

-- typedef enum YGDirection {
#num YGDirectionInherit
#num YGDirectionLTR
#num YGDirectionRTL

-- typedef enum YGLogLevel {
#num YGLogLevelError
#num YGLogLevelWarn
#num YGLogLevelInfo
#num YGLogLevelDebug
#num YGLogLevelVerbose

-- typedef enum YGWrap {
#num YGWrapNoWrap
#num YGWrapWrap

-- typedef enum YGOverflow {
#num YGOverflowVisible
#num YGOverflowHidden
#num YGOverflowScroll

-- typedef enum YGExperimentalFeature {
#num YGExperimentalFeatureWebFlexBasis

-- typedef enum YGAlign {
#num YGAlignAuto
#num YGAlignFlexStart
#num YGAlignCenter
#num YGAlignFlexEnd
#num YGAlignStretch
