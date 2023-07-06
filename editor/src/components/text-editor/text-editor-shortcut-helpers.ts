import * as EditorActions from '../editor/actions/action-creators'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ComputedStyle, emptyComments, jsExpressionValue } from '../../core/shared/element-template'
import type { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import type { EditorDispatch } from '../editor/action-types'
import { EditorAction } from '../editor/action-types'
import { MetadataUtils } from '../../core/model/element-metadata-utils'

export type UndoBehavior = 'separate-undo-step' | 'merge-with-previous'

export function toggleTextBold(
  target: ElementPath,
  fontWeight: string | null,
  dispatch: EditorDispatch,
  metadataRef: {
    readonly current: ElementInstanceMetadataMap
  },
  undoBehavior: UndoBehavior,
): void {
  const toggledFontWeight = 'bold'
  const defaultFontWeight = 'normal'
  const currentValue = fontWeight === '400' ? defaultFontWeight : toggledFontWeight

  toggleStyleProp(
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
    undoBehavior,
  )
}

export function toggleTextItalic(
  target: ElementPath,
  currentFontStyle: string | null,
  dispatch: EditorDispatch,
  metadataRef: {
    readonly current: ElementInstanceMetadataMap
  },
  undoBehavior: UndoBehavior,
): void {
  const toggledFontStyle = 'italic'
  const defaultFontStyle = 'normal'

  toggleStyleProp(
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
    undoBehavior,
  )
}

export function toggleTextUnderline(
  target: ElementPath,
  currentTextDecorationLine: string | null,
  dispatch: EditorDispatch,
  metadataRef: {
    readonly current: ElementInstanceMetadataMap
  },
  undoBehavior: UndoBehavior,
): void {
  const toggledDecoration = 'underline'
  const defaultDecoration = 'none'

  toggleStyleProp(
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
    undoBehavior,
  )
}

export function toggleTextStrikeThrough(
  target: ElementPath,
  currentTextDecorationLine: string | null,
  dispatch: EditorDispatch,
  metadataRef: {
    readonly current: ElementInstanceMetadataMap
  },
  undoBehavior: UndoBehavior,
): void {
  const toggledDecoration = 'line-through'
  const defaultDecoration = 'none'

  toggleStyleProp(
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
    undoBehavior,
  )
}

const toggleStyleProp = (
  elementPath: ElementPath,
  prop: string,
  currentValue: string | null,
  toggledValue: string,
  defaultValue: string,
  dispatch: EditorDispatch,
  getComputedValue: () => string | null,
  undoBehavior: UndoBehavior,
): void => {
  const newValue = currentValue === toggledValue ? defaultValue : toggledValue

  const setAction = EditorActions.setProp_UNSAFE(
    elementPath,
    PP.create('style', prop),
    jsExpressionValue(newValue, emptyComments),
  )

  if (newValue === defaultValue) {
    const unsetAction = EditorActions.unsetProperty(elementPath, PP.create('style', prop))

    const action =
      undoBehavior === 'separate-undo-step'
        ? unsetAction
        : EditorActions.mergeWithPrevUndo([unsetAction])

    dispatch([action])
    const computedValue = getComputedValue()
    if (computedValue !== newValue) {
      dispatch([EditorActions.mergeWithPrevUndo([setAction])])
    }
  } else {
    const action =
      undoBehavior === 'separate-undo-step'
        ? setAction
        : EditorActions.mergeWithPrevUndo([setAction])

    dispatch([action])
  }
}
