import { MetadataUtils } from '../core/model/element-metadata-utils'
import {
  ElementInstanceMetadata,
  jsxAttributeValue,
  emptyComments,
} from '../core/shared/element-template'
import { NodeModules, PropertyPath, ElementPath } from '../core/shared/project-file-types'
import Utils from '../utils/utils'
import { EditorAction, EditorDispatch, TextFormattingType } from './editor/action-types'
import * as EditorActions from './editor/actions/action-creators'
import { EditorState } from './editor/store/editor-state'
import * as PP from '../core/shared/property-path'

export function italicStyleTransform(value: 'italic' | 'normal'): boolean {
  return value === 'italic'
}

export function italicStyleReverseTransform(value: boolean): string {
  return value ? 'italic' : 'normal'
}

const ItalicPropertyArray = ['style', 'fontStyle']
export const ItalicProperty: PropertyPath = PP.create(ItalicPropertyArray)

export function isItalicText(element: ElementInstanceMetadata): boolean {
  return italicStyleTransform(Utils.pathOr('normal', ItalicPropertyArray, element.props))
}

const BoldWeight: number = 700
const NormalWeight: number = 400

export function boldStyleTransform(value: number): boolean {
  return value === BoldWeight
}

export function boldStyleReverseTransform(value: boolean): number {
  return value ? BoldWeight : NormalWeight
}

const BoldPropertyArray = ['style', 'fontWeight']
export const BoldProperty: PropertyPath = PP.create(BoldPropertyArray)

export function isBoldText(element: ElementInstanceMetadata): boolean {
  return boldStyleTransform(Utils.pathOr(NormalWeight, BoldPropertyArray, element.props))
}

export function decorationStyleTransform(value: 'underline' | 'none'): boolean {
  return value === 'underline'
}

export function decorationStyleReverseTransform(value: boolean): string {
  return value ? 'underline' : 'none'
}

const UnderlinedPropertyArray = ['style', 'textDecoration']
export const UnderlinedProperty: PropertyPath = PP.create(UnderlinedPropertyArray)

export function isUnderlinedText(element: ElementInstanceMetadata): boolean {
  return decorationStyleTransform(Utils.pathOr('none', UnderlinedPropertyArray, element.props))
}

function propertyForTextFormatting(textFormatting: TextFormattingType): PropertyPath {
  switch (textFormatting) {
    case 'bold':
      return BoldProperty
    case 'italic':
      return ItalicProperty
    case 'underline':
      return UnderlinedProperty
    default:
      const _exhaustiveCheck: never = textFormatting
      throw new Error(`Unhandled formatting of ${JSON.stringify(textFormatting)}`)
  }
}

function valueForTextFormatting(textFormatting: TextFormattingType, toggleSetting: boolean): any {
  switch (textFormatting) {
    case 'bold':
      return boldStyleReverseTransform(toggleSetting)
    case 'italic':
      return italicStyleReverseTransform(toggleSetting)
    case 'underline':
      return decorationStyleReverseTransform(toggleSetting)
    default:
      const _exhaustiveCheck: never = textFormatting
      throw new Error(`Unhandled formatting of ${JSON.stringify(textFormatting)}`)
  }
}

function actionForTextFormatting(
  textFormatting: TextFormattingType,
  targets: Array<ElementPath>,
): (newValue: any) => Array<EditorAction> {
  return (newValue: any) => {
    return targets.map((target) => {
      return EditorActions.setProp_UNSAFE(
        target,
        propertyForTextFormatting(textFormatting),
        jsxAttributeValue(newValue, emptyComments),
      )
    })
  }
}

export function toggleTextFormatting(
  editor: EditorState,
  dispatch: EditorDispatch,
  textFormatting: TextFormattingType,
): Array<EditorAction> {
  // Find all the text elements.
  let textElements: Array<ElementInstanceMetadata> = []
  const textElementPaths = editor.selectedViews.filter((selectedView) => {
    const element = MetadataUtils.findElementByElementPath(editor.jsxMetadata, selectedView)
    if (element != null && MetadataUtils.isTextAgainstImports(element)) {
      textElements.push(element)
      return true
    } else {
      return false
    }
  })

  if (textElements.length > 0) {
    // For the type of text formatting determine if enabled or disabled is the most prominent case.
    let withTextFormatting: number = 0
    Utils.fastForEach(textElements, (element) => {
      switch (textFormatting) {
        case 'bold':
          if (isBoldText(element)) {
            withTextFormatting++
          }
          break
        case 'italic':
          if (isItalicText(element)) {
            withTextFormatting++
          }
          break
        case 'underline':
          if (isUnderlinedText(element)) {
            withTextFormatting++
          }
          break
        default:
          const _exhaustiveCheck: never = textFormatting
          throw new Error(`Unhandled formatting of ${JSON.stringify(textFormatting)}`)
      }
    })

    // Flip the option to the inverse of the most prominent one.
    const optionMajorityEnabled = withTextFormatting > textElements.length / 2
    const newValue = valueForTextFormatting(textFormatting, !optionMajorityEnabled)

    return actionForTextFormatting(textFormatting, textElementPaths)(newValue)
  } else {
    return []
  }
}
