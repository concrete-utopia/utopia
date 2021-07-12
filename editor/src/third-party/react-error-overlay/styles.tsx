/**
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import { colorTheme } from '../../uuiui'

/* @flow */
const black = '#293238',
  darkGray = '#878e91',
  red = '#ce1126',
  redTransparent = 'rgba(206, 17, 38, 0.05)',
  lightRed = '#fccfcf',
  yellow = '#fbf5b4',
  yellowTransparent = 'rgba(251, 245, 180, 0.3)',
  white = '#ffffff'

const iframeStyle = {
  position: 'fixed',
  top: '0',
  left: '0',
  width: '100%',
  height: '100%',
  border: 'none',
  'z-index': 2147483647,
}

const overlayStyle = (overlayOffset: number) =>
  ({
    pointerEvents: 'initial',
    position: 'absolute',
    height: '100%',
    right: 0,
    left: overlayOffset,
    bottom: 0,
    top: 0,
    backgroundColor: colorTheme.neutralBackground.value,
  } as const)

const primaryErrorStyle = {
  'background-color': lightRed,
}

const secondaryErrorStyle = {
  'background-color': yellow,
}

export {
  iframeStyle,
  overlayStyle,
  primaryErrorStyle,
  secondaryErrorStyle,
  black,
  darkGray,
  red,
  redTransparent,
  yellowTransparent,
}
