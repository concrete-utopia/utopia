import React from 'react'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import {
  jsxConditionalExpression,
  jsxElement,
  jsxFragment,
  type JSXElement,
  type JSXElementChild,
  setJSXAttributesAttribute,
  jsExpressionValue,
  emptyComments,
  getJSXAttribute,
  type JSXAttributes,
} from '../../core/shared/element-template'
import { CanvasMousePositionRaw } from '../../utils/global-positions'
import { Modifier } from '../../utils/modifiers'
import CanvasActions from '../canvas/canvas-actions'
import {
  boundingArea,
  createHoverInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import { enableInsertModeForJSXElement, showToast } from './actions/action-creators'
import {
  defaultButtonElement,
  defaultDivElement,
  defaultImgElement,
  defaultSpanElement,
} from './defaults'
import type { InsertionSubject } from './editor-modes'
import { useDispatch } from './store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from './store/store-hook'
import type { InsertMenuItem } from '../canvas/ui/floating-insert-menu'
import {
  elementToReparent,
  getTargetParentForOneShotInsertion,
} from '../canvas/canvas-strategies/strategies/reparent-utils'
import { fixUtopiaElement, generateConsistentUID } from '../../core/shared/uid-utils'
import { getAllUniqueUids, getAllUniqueUidsFromLookup } from '../../core/model/get-unique-ids'
import { assertNever } from '../../core/shared/utils'
import type { ComponentElementToInsert } from '../custom-code/code-file'
import { notice } from '../common/notice'
import * as PP from '../../core/shared/property-path'
import { setJSXValueInAttributeAtPath } from '../../core/shared/jsx-attribute-utils'
import { defaultEither, isLeft } from '../../core/shared/either'
import { executeFirstApplicableStrategy } from '../inspector/inspector-strategies/inspector-strategy'
import { insertAsAbsoluteStrategy } from './one-shot-insertion-strategies/insert-as-absolute-strategy'
import { insertAsStaticStrategy } from './one-shot-insertion-strategies/insert-as-static-strategy'
import { getStoryboardElementPath } from '../../core/model/scene-utils'
import { type Size } from '../../core/shared/math-utils'

function shouldSubjectBeWrappedWithConditional(
  subject: InsertionSubject,
  isWrapInConditionalInsertOptionSet: boolean,
): boolean {
  return subject.insertionSubjectWrapper === 'conditional' && isWrapInConditionalInsertOptionSet
}

export function useCheckInsertModeForElementType(
  elementName: string,
  insertOptions?: {
    textEdit?: boolean
    wrapInConditional?: boolean
  },
): boolean {
  return useEditorState(
    Substores.restOfEditor,
    (store) => {
      const mode = store.editor.mode
      const isTextEditInsertOptionSet = insertOptions?.textEdit ?? false
      const isWrapInConditionalInsertOptionSet = insertOptions?.wrapInConditional ?? false
      return (
        mode.type === 'insert' &&
        mode.subjects.some(
          (subject) =>
            subject.element.type === 'JSX_ELEMENT' &&
            subject.element.name.baseVariable === elementName &&
            subject.textEdit === isTextEditInsertOptionSet &&
            shouldSubjectBeWrappedWithConditional(subject, isWrapInConditionalInsertOptionSet),
        )
      )
    },
    'useCheckInsertModeForElementType mode',
  )
}

export function useEnterDrawToInsertForDiv(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultDivElement)
}

export function useEnterTextEditMode(): (event: React.MouseEvent<Element>) => void {
  const textInsertCallbackWithTextEditing = useEnterDrawToInsertForElement(defaultSpanElement)

  return React.useCallback(
    (event: React.MouseEvent<Element>): void => {
      textInsertCallbackWithTextEditing(event, { textEdit: true })
    },
    [textInsertCallbackWithTextEditing],
  )
}

export function useEnterDrawToInsertForImage(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultImgElement)
}

export function useEnterDrawToInsertForButton(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultButtonElement)
}

export function useEnterDrawToInsertForConditional(): (event: React.MouseEvent<Element>) => void {
  const conditionalInsertCallback = useEnterDrawToInsertForElement(defaultDivElement)

  return React.useCallback(
    (event: React.MouseEvent<Element>): void => {
      conditionalInsertCallback(event, { wrapInConditional: true })
    },
    [conditionalInsertCallback],
  )
}

function useEnterDrawToInsertForElement(elementFactory: (newUID: string) => JSXElement): (
  event: React.MouseEvent<Element>,
  insertOptions?: {
    textEdit?: boolean
    wrapInConditional?: boolean
  },
) => void {
  const dispatch = useDispatch()
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)

  return React.useCallback(
    (
      event: React.MouseEvent<Element>,
      insertOptions: {
        textEdit?: boolean
        wrapInConditional?: boolean
      } = {},
    ): void => {
      const modifiers = Modifier.modifiersForEvent(event)
      const newUID = generateUidWithExistingComponents(projectContentsRef.current)

      dispatch([
        enableInsertModeForJSXElement(elementFactory(newUID), newUID, {}, null, {
          textEdit: insertOptions?.textEdit,
          wrapInContainer: insertOptions.wrapInConditional === true ? 'conditional' : undefined,
        }),
        CanvasActions.createInteractionSession(
          createHoverInteractionViaMouse(
            CanvasMousePositionRaw!,
            modifiers,
            boundingArea(),
            'zero-drag-permitted',
          ),
        ),
      ])
    },
    [dispatch, projectContentsRef, elementFactory],
  )
}

export function useToInsert(): (elementToInsert: InsertMenuItem | null) => void {
  const dispatch = useDispatch()
  const builtInDependenciesRef = useRefEditorState((store) => store.builtInDependencies)
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const jsxMetadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const openFileRef = useRefEditorState((store) => store.editor.canvas.openFile?.filename ?? null)
  const nodeModulesRef = useRefEditorState((store) => store.editor.nodeModules)
  const propertyControlsInfoRef = useRefEditorState((store) => store.editor.propertyControlsInfo)

  return React.useCallback(
    (elementToInsert: InsertMenuItem | null) => {
      if (elementToInsert == null) {
        return
      }

      const storyboardPath = getStoryboardElementPath(
        projectContentsRef.current,
        openFileRef.current,
      )
      if (storyboardPath == null) {
        // if there's no storyboard, there's not much you can do
        return
      }

      const allElementUids = new Set(
        getAllUniqueUidsFromLookup(getAllUniqueUids(projectContentsRef.current).uidsToFilePaths),
      )

      const wrappedUid = generateConsistentUID('wrapper', allElementUids)

      allElementUids.add(wrappedUid)

      const elementUid = generateConsistentUID('element', allElementUids)

      const element = elementToReparent(
        fixUtopiaElement(
          elementFromInsertMenuItem(
            elementToInsert.value.element(),
            elementUid,
            elementToInsert.value.defaultSize ?? undefined,
          ),
          allElementUids,
        ).value,
        elementToInsert.value.importsToAdd,
      )

      const targetParent = getTargetParentForOneShotInsertion(
        storyboardPath,
        projectContentsRef.current,
        selectedViewsRef.current,
        jsxMetadataRef.current,
        [element.element],
        elementPathTreeRef.current,
        elementToInsert.value.insertionCeiling,
        propertyControlsInfoRef.current,
      )

      if (isLeft(targetParent)) {
        dispatch([
          showToast(
            notice(targetParent.value, 'INFO', false, 'to-insert-does-not-support-children'),
          ),
        ])
        return
      }

      executeFirstApplicableStrategy(dispatch, [
        insertAsAbsoluteStrategy(
          element,
          jsxMetadataRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          targetParent.value.parentPath,
          builtInDependenciesRef.current,
          projectContentsRef.current,
          nodeModulesRef.current.files,
        ),
        insertAsStaticStrategy(
          element,
          jsxMetadataRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          targetParent.value.parentPath,
          builtInDependenciesRef.current,
          projectContentsRef.current,
          nodeModulesRef.current.files,
        ),
      ])
    },
    [
      allElementPropsRef,
      builtInDependenciesRef,
      dispatch,
      elementPathTreeRef,
      jsxMetadataRef,
      nodeModulesRef,
      openFileRef,
      projectContentsRef,
      selectedViewsRef,
      propertyControlsInfoRef,
    ],
  )
}
function attributesWithDefaults(initialProps: JSXAttributes, defaultSize: Size): JSXAttributes {
  const styleAttributes =
    getJSXAttribute(initialProps, 'style') ?? jsExpressionValue({}, emptyComments)

  const styleWithWidth = defaultEither(
    styleAttributes,
    setJSXValueInAttributeAtPath(
      styleAttributes,
      PP.fromString('width'),
      jsExpressionValue(defaultSize.width, emptyComments),
    ),
  )
  const styleWithHeight = defaultEither(
    styleAttributes,
    setJSXValueInAttributeAtPath(
      styleWithWidth,
      PP.fromString('height'),
      jsExpressionValue(defaultSize.height, emptyComments),
    ),
  )

  return setJSXAttributesAttribute(initialProps, 'style', styleWithHeight)
}

export function elementFromInsertMenuItem(
  element: ComponentElementToInsert,
  uid: string,
  defaultSize?: Size,
): JSXElementChild {
  switch (element.type) {
    case 'JSX_ELEMENT':
      const attributes =
        defaultSize == null ? element.props : attributesWithDefaults(element.props, defaultSize)

      const attributesWithUid = setJSXAttributesAttribute(
        attributes,
        'data-uid',
        jsExpressionValue(uid, emptyComments),
      )

      return jsxElement(element.name, uid, attributesWithUid, element.children)
    case 'JSX_CONDITIONAL_EXPRESSION':
      return jsxConditionalExpression(
        uid,
        element.condition,
        element.originalConditionString,
        element.whenTrue,
        element.whenFalse,
        element.comments,
      )
    case 'JSX_FRAGMENT':
      return jsxFragment(uid, element.children, element.longForm)
    case 'JSX_MAP_EXPRESSION':
      return { ...element, uid }
    default:
      assertNever(element)
  }
}
