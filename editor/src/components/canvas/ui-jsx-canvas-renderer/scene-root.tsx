import * as React from 'react'
import { View } from 'utopia-api'
import { useContextSelector } from 'use-context-selector'
import * as fastDeepEquals from 'fast-deep-equal'
import { getValidTemplatePaths } from '../../../core/model/element-template-utils'
import { left } from '../../../core/shared/either'
import {
  emptySpecialSizeMeasurements,
  emptyComputedStyle,
  JSXElement,
  emptyAttributeMetadatada,
  isUtopiaJSXComponent,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import {
  InstancePath,
  isParseSuccess,
  isTextFile,
  ScenePath,
} from '../../../core/shared/project-file-types'
import { colorTheme, UtopiaStyles } from '../../../uuiui'
import { UiJsxCanvasContextData, UiJsxCanvasContext } from '../ui-jsx-canvas'
import {
  ComponentRendererComponent,
  isComponentRendererComponent,
} from './ui-jsx-canvas-component-renderer'
import {
  MutableUtopiaContext,
  ParentLevelUtopiaContext,
  RerenderUtopiaContext,
  SceneLevelUtopiaContext,
  UtopiaProjectContext,
} from './ui-jsx-canvas-contexts'
import { renderComponentUsingJsxFactoryFunction } from './ui-jsx-canvas-element-renderer-utils'
import * as TP from '../../../core/shared/template-path'
import * as PP from '../../../core/shared/property-path'
import { betterReactMemo } from '../../../uuiui-deps'
import { jsxAttributesToProps } from '../../../core/shared/jsx-attributes'
import { getUtopiaIDFromJSXElement } from '../../../core/shared/uid-utils'
import utils from '../../../utils/utils'
import { PathForResizeContent } from '../../../core/model/scene-utils'
import { UTOPIA_SCENE_PATH, UTOPIA_UIDS_KEY } from '../../../core/model/utopia-constants'
import { optionalMap } from '../../../core/shared/optional-utils'
import { useEditorState } from '../../editor/store/store-hook'
import { getFileForName, getOpenUIJSFileKey } from '../../editor/store/editor-state'
import { fastForEach } from '../../../core/shared/utils'
import {
  getParseSuccessOrTransientForFilePath,
  useGetTopLevelElements,
} from './ui-jsx-canvas-top-level-elements'
import { ProjectContentTreeRoot } from '../../assets'

interface SceneProps {
  component?: React.ComponentType | null
  props?: any
  style?: React.CSSProperties
  'data-uid'?: string
  'data-label'?: string
}

function useRunSpy(
  scenePath: ScenePath,
  templatePath: InstancePath,
  componentName: string | null,
  props: SceneProps,
): void {
  const shouldIncludeCanvasRootInTheSpy = useContextSelector(
    RerenderUtopiaContext,
    (c) => c.shouldIncludeCanvasRootInTheSpy,
  )
  let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)

  const resizesContent = Boolean(utils.path(PP.getElements(PathForResizeContent), props) ?? false)

  metadataContext.current.spyValues.scenes[TP.toString(scenePath)] = {
    scenePath: scenePath,
    templatePath: templatePath,
    component: componentName,
    sceneResizesContent: resizesContent,
    globalFrame: null,
    label: props['data-label'],
    style: props.style ?? {},
  }
  if (shouldIncludeCanvasRootInTheSpy) {
    let capturedProps: {
      resizesContent?: boolean
      style?: React.CSSProperties
      'data-uid'?: string
    } = {}
    if (resizesContent) {
      capturedProps.resizesContent = true
    }
    if (props.style != null) {
      capturedProps.style = props.style
    }
    if (props['data-uid'] != null) {
      capturedProps['data-uid'] = props['data-uid']
    }

    metadataContext.current.spyValues.metadata[TP.toComponentId(templatePath)] = {
      element: left('Scene'),
      templatePath: templatePath,
      props: capturedProps,
      globalFrame: null,
      localFrame: null,
      children: [],
      rootElements: [],
      componentInstance: false,
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
      componentName: componentName,
      label: props['data-label'] ?? null,
    }
  }
}

function getTopLevelElementName(
  componentRenderer: ComponentRendererComponent | React.ComponentType | null | undefined,
): string | null {
  if (isComponentRendererComponent(componentRenderer)) {
    return componentRenderer.topLevelElementName
  } else {
    return null
  }
}

function useGetValidTemplatePaths(
  topLevelElementName: string | null,
  scenePath: ScenePath,
): Array<InstancePath> {
  const uiFilePath = useContextSelector(UtopiaProjectContext, (c) => c.openStoryboardFilePathKILLME)
  const projectContents = useContextSelector(UtopiaProjectContext, (c) => c.projectContents)
  const resolve = useContextSelector(UtopiaProjectContext, (c) => c.resolve)

  const topLevelElements = useGetTopLevelElements(uiFilePath)
  let topLevelJSXComponents: Map<string, UtopiaJSXComponent> = new Map()
  fastForEach(topLevelElements, (topLevelElement) => {
    if (isUtopiaJSXComponent(topLevelElement)) {
      topLevelJSXComponents.set(topLevelElement.name, topLevelElement)
    }
  })

  const focusedElementPath = useContextSelector(RerenderUtopiaContext, (c) => c.focusedElementPath)

  if (uiFilePath == null) {
    return []
  }

  return getValidTemplatePaths(
    focusedElementPath,
    topLevelElementName,
    TP.dynamicPathToStaticPath(scenePath),
    projectContents,
    uiFilePath,
    resolve,
  )
}

interface SceneRootRendererProps {
  sceneElement: JSXElement
  filePath: string
  style?: React.CSSProperties
}

export const SceneRootRenderer = betterReactMemo(
  'SceneRootRenderer',
  (props: SceneRootRendererProps) => {
    const mutableUtopiaContext = React.useContext(MutableUtopiaContext).current[props.filePath]
      .mutableContext
    const inScope = mutableUtopiaContext.rootScope
    const requireResult = mutableUtopiaContext.requireResult
    const canvasIsLive = useContextSelector(RerenderUtopiaContext, (c) => c.canvasIsLive)
    const parentPath = useContextSelector(ParentLevelUtopiaContext, (c) => c.templatePath)
    const uid = getUtopiaIDFromJSXElement(props.sceneElement)

    if (parentPath == null) {
      throw new Error(`Utopia Error: no parent template path provided for Scene (uid: ${uid})`)
    }

    const sceneProps: SceneProps = React.useMemo(
      () => jsxAttributesToProps(inScope, props.sceneElement.props, requireResult),
      [inScope, props.sceneElement.props, requireResult],
    )
    const templatePath = React.useMemo(() => TP.appendToPath(parentPath, uid), [parentPath, uid])
    const scenePath = TP.scenePathForElementAtInstancePath(templatePath)

    const topLevelElementName = getTopLevelElementName(sceneProps.component)

    const validPaths = useGetValidTemplatePaths(topLevelElementName, scenePath)

    useRunSpy(scenePath, templatePath, topLevelElementName, sceneProps)

    const propsWithScenePath = {
      ...sceneProps.props,
      [UTOPIA_SCENE_PATH]: scenePath,
      [UTOPIA_UIDS_KEY]: uid,
    }

    const rootElement =
      sceneProps.component == null
        ? null
        : renderComponentUsingJsxFactoryFunction(
            inScope,
            mutableUtopiaContext.jsxFactoryFunctionName,
            sceneProps.component,
            propsWithScenePath,
            undefined,
          )

    const sceneStyle: React.CSSProperties = {
      position: 'relative',
      backgroundColor: colorTheme.emphasizedBackground.value,
      boxShadow: canvasIsLive
        ? UtopiaStyles.scene.live.boxShadow
        : UtopiaStyles.scene.editing.boxShadow,
      ...props.style,
      ...sceneProps.style,
    }

    return (
      <SceneLevelUtopiaContext.Provider value={{ validPaths: validPaths }}>
        <View
          data-utopia-scene-id={TP.toString(scenePath)}
          data-utopia-valid-paths={validPaths.map(TP.toString).join(' ')}
          style={sceneStyle}
        >
          {rootElement}
        </View>
      </SceneLevelUtopiaContext.Provider>
    )
  },
  (prevProps, nextProps) => {
    // TODO BALAZS remove me, this is only needed until I fix the pragma
    return (
      prevProps.sceneElement === nextProps.sceneElement &&
      fastDeepEquals(prevProps.style, nextProps.style)
    )
  },
  true,
)
