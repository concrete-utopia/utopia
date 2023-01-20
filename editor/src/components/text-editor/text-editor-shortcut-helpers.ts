import * as EditorActions from '../editor/actions/action-creators'
import { ComputedStyle, emptyComments, jsxAttributeValue } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { EditorAction } from '../editor/action-types'

export function toggleTextBold(target: ElementPath, fontWeight: string | null): EditorAction {
  const toggledFontWeight = 'bold'
  const defaultFontWeight = 'normal'
  const currentValue = fontWeight === '400' ? defaultFontWeight : toggledFontWeight

  return toggleStyleProp(target, 'fontWeight', currentValue, toggledFontWeight, defaultFontWeight)
}

export function toggleTextItalic(
  target: ElementPath,
  currentFontStyle: string | null,
): EditorAction {
  const toggledFontStyle = 'italic'
  const defaultFontStyle = 'normal'

  return toggleStyleProp(target, 'fontStyle', currentFontStyle, toggledFontStyle, defaultFontStyle)
}

export function toggleTextUnderline(
  target: ElementPath,
  currentTextDecorationLine: string | null,
): EditorAction {
  const toggledDecoration = 'underline'
  const defaultDecoration = 'none'

  return toggleStyleProp(
    target,
    'textDecoration',
    currentTextDecorationLine,
    toggledDecoration,
    defaultDecoration,
  )
}

export function toggleTextStrikeThrough(
  target: ElementPath,
  currentTextDecorationLine: string | null,
): EditorAction {
  const toggledDecoration = 'line-through'
  const defaultDecoration = 'none'

  return toggleStyleProp(
    target,
    'textDecoration',
    currentTextDecorationLine,
    toggledDecoration,
    defaultDecoration,
  )
}

const toggleStyleProp = (
  elementPath: ElementPath,
  prop: string,
  currentValue: string | null,
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
