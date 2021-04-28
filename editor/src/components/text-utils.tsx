import * as OPI from 'object-path-immutable'
import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { Text, TextProps } from 'utopia-api'
import { MetadataUtils } from '../core/model/element-metadata-utils'
import {
  ElementInstanceMetadata,
  jsxAttributeValue,
  ElementInstanceMetadataMap,
} from '../core/shared/element-template'
import { getUtopiaID } from '../core/model/element-template-utils'
import { Imports, InstancePath, NodeModules, PropertyPath } from '../core/shared/project-file-types'
import Utils from '../utils/utils'
import { Size } from '../core/shared/math-utils'
import { EditorAction, EditorDispatch, TextFormattingType } from './editor/action-types'
import * as EditorActions from './editor/actions/action-creators'
import {
  EditorState,
  forUnderlyingTarget,
  withUnderlyingTargetFromEditorState,
} from './editor/store/editor-state'
import * as PP from '../core/shared/property-path'
import { isInstancePath } from '../core/shared/template-path'
import { emptyComments } from '../core/workers/parser-printer/parser-printer-comments'
import { ProjectContentTreeRoot } from './assets'

const ObjectPathImmutable: any = OPI

export function autosizingTextResizeNew(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null,
  metadata: ElementInstanceMetadataMap,
  targets: Array<InstancePath>,
  dispatch: EditorDispatch,
  property: PropertyPath,
  newValue: any,
  onChange: (v: any) => Array<EditorAction>,
): Array<EditorAction> {
  const onChangeActions: Array<EditorAction> = onChange(newValue)
  let promises: Array<Promise<Array<EditorAction>>> = []

  let changeAttachedToPromise: boolean = false
  Utils.fastForEach(targets, (target) => {
    const element = MetadataUtils.getElementByInstancePathMaybe(metadata, target)

    forUnderlyingTarget(target, projectContents, nodeModules, openFile, (underlyingSuccess) => {
      if (
        element != null &&
        MetadataUtils.isTextAgainstImports(underlyingSuccess.imports, element)
      ) {
        const updatedElementProps = ObjectPathImmutable.set(
          element.props,
          PP.toString(property),
          newValue,
        )
        const updatedElement = {
          ...element,
          props: updatedElementProps,
        }

        if (updatedElement.props.textSizing === 'auto') {
          changeAttachedToPromise = true
          promises.push(
            measureTextFieldNew(updatedElement).then((size: Size) => {
              return [EditorActions.updateFrameDimensions(target, size.width, size.height)]
            }),
          )
        }
      }
    })
  })

  Promise.all(promises).then((actionArrays) => {
    let actions = Utils.flatMapArray((array) => array, actionArrays)
    if (changeAttachedToPromise) {
      actions = [...onChangeActions, ...actions]
    }
    dispatch(actions, 'canvas')
  })

  if (changeAttachedToPromise) {
    return []
  } else {
    return onChangeActions
  }
}

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
  targets: Array<InstancePath>,
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
  const instancePaths = editor.selectedViews.filter(isInstancePath)
  let textElements: Array<ElementInstanceMetadata> = []
  const textElementPaths = instancePaths.filter((selectedView) => {
    return withUnderlyingTargetFromEditorState(selectedView, editor, false, (underlyingSuccess) => {
      const element = MetadataUtils.getElementByInstancePathMaybe(editor.jsxMetadata, selectedView)
      if (
        element != null &&
        MetadataUtils.isTextAgainstImports(underlyingSuccess.imports, element)
      ) {
        textElements.push(element)
        return true
      } else {
        return false
      }
    })
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

    // Property to modify.
    const propertyToModify = propertyForTextFormatting(textFormatting)

    // Flip the option to the inverse of the most prominent one.
    const optionMajorityEnabled = withTextFormatting > textElements.length / 2
    const newValue = valueForTextFormatting(textFormatting, !optionMajorityEnabled)

    return autosizingTextResizeNew(
      editor.projectContents,
      editor.nodeModules.files,
      editor.canvas.openFile?.filename ?? null,
      editor.jsxMetadata,
      textElementPaths,
      dispatch,
      propertyToModify,
      newValue,
      actionForTextFormatting(textFormatting, textElementPaths),
    )
  } else {
    return []
  }
}

export async function measureTextFieldNew(element: ElementInstanceMetadata): Promise<Size> {
  const containerNode = document.getElementById('text-field-placeholder')
  return new Promise<Size>((resolve, reject) => {
    const textFieldProps = {
      ...(element.props as TextProps),
      key: `textmeasurement-${getUtopiaID(element)}`,
      componentSizeResult: (width: number | null, height: number | null) => {
        if (containerNode != null) {
          ReactDOM.unmountComponentAtNode(containerNode)
        }
        resolve({ width: width, height: height } as Size)
      },
      scale: 1,
    }
    const textElement = <Text {...textFieldProps} />
    ReactDOM.render(textElement, containerNode)
  })
}
