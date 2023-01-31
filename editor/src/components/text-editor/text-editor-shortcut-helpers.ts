import * as EditorActions from '../editor/actions/action-creators'
import {
  ComputedStyle,
  ElementInstanceMetadataMap,
  emptyComments,
  jsxAttributeValue,
} from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { EditorAction, EditorDispatch } from '../editor/action-types'
import { MetadataUtils } from '../../core/model/element-metadata-utils'

export function toggleTextBold(target: ElementPath, fontWeight: string | null): EditorAction {
  const toggledFontWeight = 'bold'
  const defaultFontWeight = 'normal'
  const currentValue = fontWeight === '400' ? defaultFontWeight : toggledFontWeight

  return toggleStyleProp(target, 'fontWeight', currentValue, toggledFontWeight, defaultFontWeight)
}

export function toggleTextBoldWithUnset(
  target: ElementPath,
  fontWeight: string | null,
  dispatch: EditorDispatch,
  metadataRef: {
    readonly current: ElementInstanceMetadataMap
  },
): void {
  const toggledFontWeight = 'bold'
  const defaultFontWeight = 'normal'
  const currentValue = fontWeight === '400' ? defaultFontWeight : toggledFontWeight

  toggleStylePropWithUnset(
    target,
    'fontWeight',
    currentValue,
    toggledFontWeight,
    defaultFontWeight,
    dispatch,
    () => {
      const specialSizeMeasurements = MetadataUtils.findElementByElementPath(
        metadataRef.current,
        target,
      )?.specialSizeMeasurements
      if (specialSizeMeasurements?.fontWeight === '400') {
        return 'normal'
      }
      return specialSizeMeasurements?.fontWeight ?? null
    },
  )
}

export function toggleTextItalic(
  target: ElementPath,
  currentFontStyle: string | null,
): EditorAction {
  const toggledFontStyle = 'italic'
  const defaultFontStyle = 'normal'

  return toggleStyleProp(target, 'fontStyle', currentFontStyle, toggledFontStyle, defaultFontStyle)
}

export function toggleTextItalicWithUnset(
  target: ElementPath,
  currentFontStyle: string | null,
  dispatch: EditorDispatch,
  metadataRef: {
    readonly current: ElementInstanceMetadataMap
  },
): void {
  const toggledFontStyle = 'italic'
  const defaultFontStyle = 'normal'

  toggleStylePropWithUnset(
    target,
    'fontStyle',
    currentFontStyle,
    toggledFontStyle,
    defaultFontStyle,
    dispatch,
    () => {
      const specialSizeMeasurements = MetadataUtils.findElementByElementPath(
        metadataRef.current,
        target,
      )?.specialSizeMeasurements
      return specialSizeMeasurements?.fontStyle ?? null
    },
  )
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

export function toggleTextUnderlineWithUnset(
  target: ElementPath,
  currentTextDecorationLine: string | null,
  dispatch: EditorDispatch,
  metadataRef: {
    readonly current: ElementInstanceMetadataMap
  },
): void {
  const toggledDecoration = 'underline'
  const defaultDecoration = 'none'

  toggleStylePropWithUnset(
    target,
    'textDecoration',
    currentTextDecorationLine,
    toggledDecoration,
    defaultDecoration,
    dispatch,
    () => {
      const specialSizeMeasurements = MetadataUtils.findElementByElementPath(
        metadataRef.current,
        target,
      )?.specialSizeMeasurements
      return specialSizeMeasurements?.textDecorationLine ?? null
    },
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

export function toggleTextStrikeThroughWithUnset(
  target: ElementPath,
  currentTextDecorationLine: string | null,
  dispatch: EditorDispatch,
  metadataRef: {
    readonly current: ElementInstanceMetadataMap
  },
): void {
  const toggledDecoration = 'line-through'
  const defaultDecoration = 'none'

  toggleStylePropWithUnset(
    target,
    'textDecoration',
    currentTextDecorationLine,
    toggledDecoration,
    defaultDecoration,
    dispatch,
    () => {
      const specialSizeMeasurements = MetadataUtils.findElementByElementPath(
        metadataRef.current,
        target,
      )?.specialSizeMeasurements
      return specialSizeMeasurements?.textDecorationLine ?? null
    },
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
    PP.create('style', prop),
    jsxAttributeValue(newValue, emptyComments),
  )
}

const toggleStylePropWithUnset = (
  elementPath: ElementPath,
  prop: string,
  currentValue: string | null,
  toggledValue: string,
  defaultValue: string,
  dispatch: EditorDispatch,
  getComputedValue: () => string | null,
): void => {
  const newValue = currentValue === toggledValue ? defaultValue : toggledValue
  if (newValue === defaultValue) {
    dispatch([EditorActions.unsetProperty(elementPath, PP.create('style', prop))])
    const computedValue = getComputedValue()
    if (computedValue !== newValue) {
      dispatch([
        EditorActions.transientActions([
          EditorActions.setProperty(
            elementPath,
            PP.create('style', prop),
            jsxAttributeValue(newValue, emptyComments),
          ),
        ]),
      ])
    }
  } else {
    dispatch([
      EditorActions.setProperty(
        elementPath,
        PP.create('style', prop),
        jsxAttributeValue(newValue, emptyComments),
      ),
    ])
  }
}
