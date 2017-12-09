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
#num YGFlexDirectionCount

-- typedef enum YGMeasureMode {
#num YGMeasureModeUndefined
#num YGMeasureModeExactly
#num YGMeasureModeAtMost
#num YGMeasureModeCount

-- typedef enum YGPrintOptions {
#num YGPrintOptionsLayout
#num YGPrintOptionsStyle
#num YGPrintOptionsChildren
#num YGPrintOptionsCount

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
#num YGEdgeCount

-- typedef enum YGPositionType {
#num YGPositionTypeRelative
#num YGPositionTypeAbsolute
#num YGPositionTypeCount

-- typedef enum YGDimension {
#num YGDimensionWidth
#num YGDimensionHeight
#num YGDimensionCount

-- typedef enum YGJustify {
#num YGJustifyFlexStart
#num YGJustifyCenter
#num YGJustifyFlexEnd
#num YGJustifySpaceBetween
#num YGJustifySpaceAround
#num YGJustifyCount

-- typedef enum YGDirection {
#num YGDirectionInherit
#num YGDirectionLTR
#num YGDirectionRTL
#num YGDirectionCount

-- typedef enum YGLogLevel {
#num YGLogLevelError
#num YGLogLevelWarn
#num YGLogLevelInfo
#num YGLogLevelDebug
#num YGLogLevelVerbose
#num YGLogLevelCount

-- typedef enum YGWrap {
#num YGWrapNoWrap
#num YGWrapWrap
#num YGWrapCount

-- typedef enum YGOverflow {
#num YGOverflowVisible
#num YGOverflowHidden
#num YGOverflowScroll
#num YGOverflowCount

-- typedef enum YGExperimentalFeature {
#num YGExperimentalFeatureWebFlexBasis
#num YGExperimentalFeatureCount

-- typedef enum YGAlign {
#num YGAlignAuto
#num YGAlignFlexStart
#num YGAlignCenter
#num YGAlignFlexEnd
#num YGAlignStretch
#num YGAlignCount
