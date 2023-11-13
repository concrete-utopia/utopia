import React from 'react'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
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
} from '../../core/shared/element-template'
import { CanvasMousePositionRaw } from '../../utils/global-positions'
import { Modifier } from '../../utils/modifiers'
import CanvasActions from '../canvas/canvas-actions'
import {
  boundingArea,
  createHoverInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import {
  applyCommandsAction,
  enableInsertModeForJSXElement,
  selectComponents,
  showToast,
} from './actions/action-creators'
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
import type {
  ElementToReparent,
  ToReparent,
} from '../canvas/canvas-strategies/strategies/reparent-utils'
import {
  elementToReparent,
  getTargetParentForOneShotInsertion,
} from '../canvas/canvas-strategies/strategies/reparent-utils'
import { fixUtopiaElement, generateConsistentUID } from '../../core/shared/uid-utils'
import { getAllUniqueUids } from '../../core/model/get-unique-ids'
import { assertNever } from '../../core/shared/utils'
import type { ComponentElementToInsert } from '../custom-code/code-file'
import { notice } from '../common/notice'
import * as PP from '../../core/shared/property-path'
import { setJSXValueInAttributeAtPath } from '../../core/shared/jsx-attributes'
import { defaultEither, isLeft } from '../../core/shared/either'
import { resultForFirstApplicableStrategy } from '../inspector/inspector-strategies/inspector-strategy'
import { insertAsAbsoluteStrategy } from './one-shot-insertion-strategies/insert-as-absolute-strategy'
import { insertAsStaticStrategy } from './one-shot-insertion-strategies/insert-as-static-strategy'
import { getStoryboardElementPath } from '../../core/model/scene-utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import type { CanvasPoint } from '../../core/shared/math-utils'
import type { CanvasCommand } from '../canvas/commands/commands'
import type { InsertionPath } from './store/insertion-path'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import type { IndexPosition } from '../../utils/utils'
import type { AllElementProps, EditorStateNodeModules } from './store/editor-state'
import type { BuiltInDependencies } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import type { ProjectContentTreeRoot } from '../assets'

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

interface insertWithStrategiesResult {
  commands: CanvasCommand[]
  newPath: ElementPath
}

export function insertWithStrategies(
  element: ToReparent,
  targetParent: InsertionPath,
  editorContext: {
    metadata: ElementInstanceMetadataMap
    elementPathTree: ElementPathTrees
    allElementProps: AllElementProps
    selectedViews: ElementPath[]
    projectContents: ProjectContentTreeRoot
    nodeModules: EditorStateNodeModules
    builtInDependencies: BuiltInDependencies
  },
  insertionContext: Partial<{
    position: CanvasPoint
    indexPosition: IndexPosition
  }>,
): insertWithStrategiesResult | null {
  const result = resultForFirstApplicableStrategy([
    insertAsAbsoluteStrategy(
      element,
      editorContext.metadata,
      editorContext.elementPathTree,
      editorContext.allElementProps,
      targetParent,
      editorContext.builtInDependencies,
      editorContext.projectContents,
      editorContext.nodeModules.files,
      {
        indexPosition: insertionContext.indexPosition,
        toPosition: insertionContext.position,
      },
    ),
    insertAsStaticStrategy(
      element,
      editorContext.metadata,
      editorContext.elementPathTree,
      editorContext.allElementProps,
      targetParent,
      editorContext.builtInDependencies,
      editorContext.projectContents,
      editorContext.nodeModules.files,
      {
        indexPosition: insertionContext.indexPosition,
      },
    ),
  ])

  if (result == null) {
    return null
  }
  return { commands: result.commands, newPath: result.data.newPath }
}

export function elementFromInsertItem(
  projectContents: ProjectContentTreeRoot,
  elementToInsert: InsertMenuItem,
): JSXElementChild {
  const allElementUids = new Set(getAllUniqueUids(projectContents).uniqueIDs)

  const elementUid = generateConsistentUID('wrapper', allElementUids)

  const element = fixUtopiaElement(
    elementFromInsertMenuItem(elementToInsert.value.element(), elementUid),
    allElementUids,
  ).value

  return element
}

export function useToInsert(): (elementToInsert: InsertMenuItem | null) => ElementPath | null {
  const dispatch = useDispatch()
  const builtInDependenciesRef = useRefEditorState((store) => store.builtInDependencies)
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const jsxMetadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const openFileRef = useRefEditorState((store) => store.editor.canvas.openFile?.filename ?? null)
  const nodeModulesRef = useRefEditorState((store) => store.editor.nodeModules)

  return React.useCallback(
    (elementToInsert: InsertMenuItem | null): ElementPath | null => {
      if (elementToInsert == null) {
        return null
      }

      const storyboardPath = getStoryboardElementPath(
        projectContentsRef.current,
        openFileRef.current,
      )
      if (storyboardPath == null) {
        // if there's no storyboard, there's not much you can do
        return null
      }

      const element = elementToReparent(
        elementFromInsertItem(projectContentsRef.current, elementToInsert),
        elementToInsert.value.importsToAdd,
      )

      const targetParent = getTargetParentForOneShotInsertion(
        storyboardPath,
        projectContentsRef.current,
        selectedViewsRef.current,
        jsxMetadataRef.current,
        [element.element],
        elementPathTreeRef.current,
      )

      if (isLeft(targetParent)) {
        dispatch([
          showToast(
            notice(targetParent.value, 'INFO', false, 'to-insert-does-not-support-children'),
          ),
        ])
        return null
      }

      const result = insertWithStrategies(
        element,
        targetParent.value.parentPath,
        {
          metadata: jsxMetadataRef.current,
          elementPathTree: elementPathTreeRef.current,
          allElementProps: allElementPropsRef.current,
          selectedViews: selectedViewsRef.current,
          projectContents: projectContentsRef.current,
          builtInDependencies: builtInDependenciesRef.current,
          nodeModules: nodeModulesRef.current,
        },
        {},
      )

      if (result == null) {
        return null
      }

      dispatch([applyCommandsAction(result.commands), selectComponents([result.newPath], false)])
      return result.newPath
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
    ],
  )
}

function elementFromInsertMenuItem(
  element: ComponentElementToInsert,
  uid: string,
): JSXElementChild {
  switch (element.type) {
    case 'JSX_ELEMENT':
      const styleAttributes =
        getJSXAttribute(element.props, 'style') ?? jsExpressionValue({}, emptyComments)

      const styleWithWidth = defaultEither(
        styleAttributes,
        setJSXValueInAttributeAtPath(
          styleAttributes,
          PP.fromString('width'),
          jsExpressionValue(100, emptyComments),
        ),
      )
      const styleWithHeight = defaultEither(
        styleAttributes,
        setJSXValueInAttributeAtPath(
          styleWithWidth,
          PP.fromString('height'),
          jsExpressionValue(100, emptyComments),
        ),
      )

      const attributesWithStyle = setJSXAttributesAttribute(element.props, 'style', styleWithHeight)
      const attributesWithUid = setJSXAttributesAttribute(
        attributesWithStyle,
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
    default:
      assertNever(element)
  }
}
