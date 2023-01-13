import * as EditorActions from '../editor/actions/action-creators'
import { ComputedStyle, emptyComments, jsxAttributeValue } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { EditorAction } from '../editor/action-types'

export function toggleTextBold(target: ElementPath, computedStyle: ComputedStyle): EditorAction {
  const toggledFontWeight = 'bold'
  const defaultFontWeight = 'normal'
  const currentValue = computedStyle['fontWeight'] === '400' ? defaultFontWeight : toggledFontWeight

  return toggleStyleProp(target, 'fontWeight', currentValue, toggledFontWeight, defaultFontWeight)
}

export function toggleTextItalic(target: ElementPath, computedStyle: ComputedStyle): EditorAction {
  const toggledFontStyle = 'italic'
  const defaultFontStyle = 'normal'
  const currentValue = computedStyle['fontStyle']

  return toggleStyleProp(target, 'fontStyle', currentValue, toggledFontStyle, defaultFontStyle)
}

export function toggleTextUnderline(
  target: ElementPath,
  computedStyle: ComputedStyle,
): EditorAction {
  const toggledDecoration = 'underline'
  const defaultDecoration = 'none'
  const currentValue = computedStyle['textDecorationLine']

  return toggleStyleProp(
    target,
    'textDecoration',
    currentValue,
    toggledDecoration,
    defaultDecoration,
  )
}

export function toggleTextStrikeThrough(
  target: ElementPath,
  computedStyle: ComputedStyle,
): EditorAction {
  const toggledDecoration = 'line-through'
  const defaultDecoration = 'none'
  const currentValue = computedStyle['textDecorationLine']

  return toggleStyleProp(
    target,
    'textDecoration',
    currentValue,
    toggledDecoration,
    defaultDecoration,
  )
}

const toggleStyleProp = (
  elementPath: ElementPath,
  prop: string,
  currentValue: string,
  toggledValue: string,
  defaultValue: string,
): EditorAction => {
  const newValue = currentValue === toggledValue ? defaultValue : toggledValue
  return EditorActions.setProperty(
    elementPath,
    PP.create(['style', prop]),
    jsxAttributeValue(newValue, emptyComments),
  )
}
