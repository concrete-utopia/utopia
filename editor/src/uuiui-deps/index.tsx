import type { ResizableProps as _ResizableProps } from 're-resizable'
import { Resizable as _Resizable } from 're-resizable'

import _onClickOutside from 'react-onclickoutside'

import _utils from '../utils/utils'
import * as _utils_star from '../utils/utils'
import * as _user from '../common/user'
import * as _common_utils from '../core/shared/utils'
import * as _math_utils from '../core/shared/math-utils'
import type { ControlStatus, PropertyStatus } from '../components/inspector/common/control-status'
import type { ControlStyles } from '../components/inspector/common/control-styles'
import {
  isControlledStatus,
  getControlStatusFromPropertyStatus,
  calculateMultiPropertyStatusForSelection,
  calculateMultiStringPropertyStatusForSelection,
} from '../components/inspector/common/control-status'
import { getControlStyles } from '../components/inspector/common/control-styles'
import * as _react_performance from '../utils/react-performance'
import * as CSSUtils from '../components/inspector/common/css-utils'
import * as EitherUtils from '../core/shared/either'
import { CSSCursor } from '../components/canvas/canvas-types'
import type {
  OnSubmitValue,
  OnSubmitValueOrEmpty,
  OnSubmitValueOrUnknownOrEmpty,
} from '../components/inspector/controls/control'
import { usePropControlledState_DEPRECATED } from '../components/inspector/common/inspector-utils'
import * as InspectorHooks from '../components/inspector/common/property-path-hooks'
import * as InspectorContextMenuItems from '../components/inspector/common/context-menu-items'

export const Resizable = _Resizable
export type ResizableProps = _ResizableProps
export const Utils = { ..._utils, ..._utils_star }
export const User = _user
export type LoginState = _user.LoginState
export const CommonUtils = _common_utils
export const MathUtils = _math_utils
export const ReactPerformance = _react_performance
export const onClickOutside = _onClickOutside

export type {
  ControlStyles,
  ControlStatus,
  PropertyStatus,
  OnSubmitValue,
  OnSubmitValueOrEmpty,
  OnSubmitValueOrUnknownOrEmpty,
}

export {
  isControlledStatus,
  getControlStyles,
  getControlStatusFromPropertyStatus,
  calculateMultiPropertyStatusForSelection,
  calculateMultiStringPropertyStatusForSelection,
  CSSUtils,
  EitherUtils,
  CSSCursor,
  usePropControlledState_DEPRECATED,
  InspectorHooks,
  InspectorContextMenuItems,
}

// These exports are experimental, to be able to make an OpacitySubsection inside Utopia
export * from '../components/inspector/widgets/property-row'
export * from '../components/inspector/controls/lightselect-control'
export * from '../components/inspector/controls/slider-control'
export * from '../components/context-menu-wrapper'
export type { SelectOption } from '../components/inspector/controls/select-control'
