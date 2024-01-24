import Hash from 'object-hash'
import type {
  SceneMetadata,
  StaticElementPath,
  PropertyPath,
  TextFile,
} from '../shared/project-file-types'
import { isTextFile, isParseSuccess } from '../shared/project-file-types'
import type {
  UtopiaJSXComponent,
  JSXElement,
  JSXElementChild,
  TopLevelElement,
  JSXAttributes,
} from '../shared/element-template'
import {
  utopiaJSXComponent,
  jsxElement,
  jsExpressionValue,
  isJSXElement,
  isUtopiaJSXComponent,
  defaultPropsParam,
  jsExpressionOtherJavaScript,
  ElementInstanceMetadataMap,
  jsxAttributesFromMap,
  ElementInstanceMetadata,
  walkElements,
  emptyComments,
  getJSXAttribute,
} from '../shared/element-template'
import * as EP from '../shared/element-path'
import * as PP from '../shared/property-path'
import {
  eitherToMaybe,
  applicative5Either,
  flatMapEither,
  isLeft,
  applicative6Either,
  right,
} from '../shared/either'
import { memoize } from '../shared/memoize'
import fastDeepEqual from 'fast-deep-equal'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  setJSXValueAtPath,
} from '../shared/jsx-attributes'
import { stripNulls } from '../shared/array-utils'
import { UTOPIA_UID_KEY } from './utopia-constants'
import type { ProjectContentTreeRoot } from '../../components/assets'
import {
  addFileToProjectContents,
  getContentsTreeFromPath,
  getProjectFileByFilePath,
} from '../../components/assets'
import {
  getUtopiaJSXComponentsFromSuccess,
  isRemixSceneAgainstImports,
  isSceneAgainstImports,
} from './project-file-utils'
import { generateConsistentUID, generateUID, getUtopiaID } from '../shared/uid-utils'
import { emptySet } from '../shared/set-utils'
import { IS_TEST_ENVIRONMENT } from '../../common/env-vars'
import type { EditorState } from '../../components/editor/store/editor-state'
import { StoryboardFilePath } from '../../components/editor/store/editor-state'
import { transformJSXElementChildRecursively } from './element-template-utils'

export const PathForSceneComponent = PP.create('component')
export const PathForSceneDataUid = PP.create('data-uid')
export const PathForSceneDataLabel = PP.create('data-label')
export const PathForSceneFrame = PP.create('style')

export const BakedInStoryboardUID = 'utopia-storyboard-uid'
export const BakedInStoryboardVariableName = 'storyboard'

export const EmptyUtopiaCanvasComponent = convertScenesToUtopiaCanvasComponent([])

export const PathForSceneProps = PP.create('props')
export const PathForSceneStyle = PP.create('style')

export function createSceneUidFromIndex(sceneIndex: number): string {
  return `scene-${sceneIndex}`
}

export function mapScene(scene: SceneMetadata): JSXElement {
  const sceneProps = jsxAttributesFromMap({
    component: jsExpressionOtherJavaScript(
      scene.component ?? 'null',
      scene.component ?? 'null',
      `return ${scene.component}`,
      [],
      null,
      {},
      emptyComments,
    ),
    props: jsExpressionValue(scene.props, emptyComments),
    style: jsExpressionValue(scene.frame, emptyComments),
    'data-uid': jsExpressionValue(scene.uid, emptyComments),
    'data-label': jsExpressionValue(scene.label, emptyComments),
  })
  return jsxElement('Scene', scene.uid, sceneProps, [])
}

export function unmapScene(element: JSXElementChild): SceneMetadata | null {
  if (!isJSXElement(element) || element.name.baseVariable !== 'Scene') {
    return null
  }
  return eitherToMaybe(
    applicative6Either(
      (component, props, style, label, uid) => {
        const scene: SceneMetadata = {
          uid: uid,
          component: component,
          props: props,
          frame: style,
          ...(label == null ? {} : { label: label }),
        }
        return scene
      },
      getSimpleAttributeAtPathCustom(element.props, PP.create('component')),
      getSimpleAttributeAtPathCustom(element.props, PP.create('props')),
      getSimpleAttributeAtPathCustom(element.props, PP.create('style')),
      getSimpleAttributeAtPathCustom(element.props, PP.create('layout')),
      getSimpleAttributeAtPathCustom(element.props, PP.create('data-label')),
      getSimpleAttributeAtPathCustom(element.props, PP.create('data-uid')),
    ),
  )
}

export function convertScenesToUtopiaCanvasComponent(
  scenes: Array<SceneMetadata>,
): UtopiaJSXComponent
export function convertScenesToUtopiaCanvasComponent(scenes: null): null
export function convertScenesToUtopiaCanvasComponent(
  scenes: Array<SceneMetadata> | null,
): UtopiaJSXComponent | null
export function convertScenesToUtopiaCanvasComponent(
  scenes: Array<SceneMetadata> | null,
): UtopiaJSXComponent | null {
  if (scenes == null) {
    return null
  }
  return utopiaJSXComponent(
    BakedInStoryboardVariableName,
    false,
    'var',
    'block',
    null,
    [],
    jsxElement(
      'Storyboard',
      BakedInStoryboardUID,
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue(BakedInStoryboardUID, emptyComments) }),
      scenes.map(mapScene),
    ),
    null,
    false,
    emptyComments,
  )
}

export function createSceneFromComponent(
  filePath: string,
  componentImportedAs: string,
  uid: string,
): JSXElement {
  const sceneProps = jsxAttributesFromMap({
    [UTOPIA_UID_KEY]: jsExpressionValue(uid, emptyComments),
    style: jsExpressionValue(
      {
        position: 'absolute',
        left: 0,
        top: 0,
        width: 375,
        height: 812,
      },
      emptyComments,
    ),
  })
  const hash = Hash({
    fileName: filePath,
    name: componentImportedAs,
    props: jsxAttributesFromMap({}),
  })
  const componentUID = generateConsistentUID(hash)
  return jsxElement('Scene', uid, sceneProps, [
    jsxElement(
      componentImportedAs,
      componentUID,
      jsxAttributesFromMap({
        [UTOPIA_UID_KEY]: jsExpressionValue(componentUID, emptyComments),
      }),
      [],
    ),
  ])
}

export function createStoryboardElement(scenes: Array<JSXElement>, uid: string): JSXElement {
  const storyboardProps = jsxAttributesFromMap({
    [UTOPIA_UID_KEY]: jsExpressionValue(uid, emptyComments),
  })
  return jsxElement('Storyboard', uid, storyboardProps, scenes)
}

export function convertUtopiaCanvasComponentToScenes(
  utopiaCanvasComponent: UtopiaJSXComponent | null,
): Array<SceneMetadata> | null {
  if (utopiaCanvasComponent == null) {
    return null
  }
  const rootElement = utopiaCanvasComponent.rootElement
  if (!isJSXElement(rootElement) || rootElement.name.baseVariable !== 'Storyboard') {
    throw new Error('the root element must be a Storyboard component')
  }
  const firstChildrenScenes: Array<SceneMetadata> = stripNulls(rootElement.children.map(unmapScene))
  return firstChildrenScenes
}

const convertScenesAndTopLevelElementsToUtopiaCanvasComponentMemoized = memoize(
  (scenes: Array<SceneMetadata>, topLevelElements: Array<TopLevelElement>) => [
    ...topLevelElements,
    convertScenesToUtopiaCanvasComponent(scenes),
  ],
  { matchesArg: fastDeepEqual }, // delete me as soon as possible
)

export function convertScenesAndTopLevelElementsToUtopiaCanvasComponent(
  scenes: Array<SceneMetadata>,
  topLevelElements: Array<UtopiaJSXComponent>,
): Array<UtopiaJSXComponent>
export function convertScenesAndTopLevelElementsToUtopiaCanvasComponent(
  scenes: Array<SceneMetadata>,
  topLevelElements: Array<TopLevelElement>,
): Array<TopLevelElement>
export function convertScenesAndTopLevelElementsToUtopiaCanvasComponent(
  scenes: Array<SceneMetadata>,
  topLevelElements: Array<TopLevelElement>,
): Array<TopLevelElement> {
  return convertScenesAndTopLevelElementsToUtopiaCanvasComponentMemoized(scenes, topLevelElements)
}

export function fishOutUtopiaCanvasFromTopLevelElements(
  topLevelElements: Array<TopLevelElement>,
): UtopiaJSXComponent | null {
  return (
    topLevelElements.find((e): e is UtopiaJSXComponent => {
      return isUtopiaJSXComponent(e) && e.name === BakedInStoryboardVariableName
    }) ?? null
  )
}

function getSimpleAttributeAtPathCustom(attributes: JSXAttributes, path: PropertyPath) {
  const getAttrResult = getModifiableJSXAttributeAtPath(attributes, path)
  return flatMapEither((attr) => {
    const simpleValue = jsxSimpleAttributeToValue(attr)
    if (isLeft(simpleValue) && attr.type === 'ATTRIBUTE_OTHER_JAVASCRIPT') {
      return right(attr.javascript)
    } else {
      return simpleValue
    }
  }, getAttrResult)
}

export function sceneMetadata(
  uid: string,
  component: string | null,
  props: { [key: string]: any },
  frame: {
    left: number
    top: number
    width: number
    height: number
  },
  label?: string,
): SceneMetadata {
  let scene: SceneMetadata = {
    uid: uid,
    component: component,
    frame: frame,
    props: props,
  }

  if (label != null) {
    // This is annoying, but if we don't do this then we'll explicitly print
    // `undefined` in the label field, meaning the JSON won't parse
    scene.label = label
  }

  return scene
}

export function getStoryboardUID(openComponents: UtopiaJSXComponent[]): string | null {
  const possiblyStoryboard = openComponents.find(
    (component) => component.name === BakedInStoryboardVariableName,
  )
  if (possiblyStoryboard != null) {
    return getUtopiaID(possiblyStoryboard.rootElement)
  }
  return null
}

export function getStoryboardElementPath(
  projectContents: ProjectContentTreeRoot,
  openFile: string | null | undefined,
): StaticElementPath | null {
  if (openFile != null) {
    const file = getProjectFileByFilePath(projectContents, openFile)
    if (file != null && isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
      const possiblyStoryboard = getUtopiaJSXComponentsFromSuccess(file.fileContents.parsed).find(
        (component) => component.name === BakedInStoryboardVariableName,
      )
      if (possiblyStoryboard != null) {
        const uid = getUtopiaID(possiblyStoryboard.rootElement)
        return EP.elementPath([EP.staticElementPath([uid])])
      }
    }
  }
  return null
}

const IdPropName = 'id'

function getIdPropFromJSXElement(element: JSXElement): string | null {
  const maybeIdProp = getJSXAttribute(element.props, IdPropName)
  if (
    maybeIdProp == null ||
    maybeIdProp.type !== 'ATTRIBUTE_VALUE' ||
    typeof maybeIdProp.value !== 'string'
  ) {
    return null
  }
  return maybeIdProp.value
}

function setIdPropOnJSXElement(element: JSXElement, idPropValueToUse: string): JSXElement | null {
  const updatedProps = setJSXValueAtPath(
    element.props,
    PP.create(IdPropName),
    jsExpressionValue(idPropValueToUse, emptyComments),
  )

  if (IS_TEST_ENVIRONMENT || isLeft(updatedProps)) {
    return null
  }
  return { ...element, props: updatedProps.value }
}

export function ensureSceneIdsExist(editor: EditorState): EditorState {
  const storyboardFile = getContentsTreeFromPath(editor.projectContents, StoryboardFilePath)
  if (
    storyboardFile == null ||
    storyboardFile.type !== 'PROJECT_CONTENT_FILE' ||
    storyboardFile.content.type !== 'TEXT_FILE' ||
    storyboardFile.content.fileContents.parsed.type !== 'PARSE_SUCCESS'
  ) {
    return editor
  }

  let seenIdProps: Set<string> = new Set()
  let anyIdPropUpdated = false
  const imports = storyboardFile.content.fileContents.parsed.imports

  const nextToplevelElements = storyboardFile.content.fileContents.parsed.topLevelElements.map(
    (e) => {
      if (e.type !== 'UTOPIA_JSX_COMPONENT') {
        return e
      }

      const nextRootElement = transformJSXElementChildRecursively(e.rootElement, (child) => {
        const isConsideredScene =
          isSceneAgainstImports(child, imports) || isRemixSceneAgainstImports(child, imports)

        if (child.type !== 'JSX_ELEMENT' || !isConsideredScene) {
          return child
        }

        const idPropValue = getIdPropFromJSXElement(child)

        if (idPropValue != null && !seenIdProps.has(idPropValue)) {
          seenIdProps.add(idPropValue)
          return child
        }

        const idPropValueToUse = child.uid
        const updatedChild = setIdPropOnJSXElement(child, idPropValueToUse)
        if (updatedChild == null) {
          return child
        }

        seenIdProps.add(idPropValueToUse)
        anyIdPropUpdated = true
        return updatedChild
      })

      return { ...e, rootElement: nextRootElement }
    },
  )

  if (!anyIdPropUpdated) {
    return editor
  }

  const nextStoryboardFile: TextFile = {
    ...storyboardFile.content,
    fileContents: {
      ...storyboardFile.content.fileContents,
      parsed: {
        ...storyboardFile.content.fileContents.parsed,
        topLevelElements: nextToplevelElements,
      },
    },
  }

  const nextProjectContents = addFileToProjectContents(
    editor.projectContents,
    StoryboardFilePath,
    nextStoryboardFile,
  )
  return { ...editor, projectContents: nextProjectContents }
}
