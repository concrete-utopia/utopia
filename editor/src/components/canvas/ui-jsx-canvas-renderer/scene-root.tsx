import * as React from 'react'
import { View } from 'utopia-api'
import { useContextSelector } from 'use-context-selector'
import * as fastDeepEquals from 'fast-deep-equal'
import { getUtopiaID, getValidTemplatePaths } from '../../../core/model/element-template-utils'
import { left } from '../../../core/shared/either'
import {
  emptySpecialSizeMeasurements,
  emptyComputedStyle,
  JSXElement,
  emptyAttributeMetadatada,
  isUtopiaJSXComponent,
  UtopiaJSXComponent,
  isJSXElement,
  getJSXElementNameAsString,
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
import {
  renderComponentUsingJsxFactoryFunction,
  renderCoreElement,
} from './ui-jsx-canvas-element-renderer-utils'
import * as TP from '../../../core/shared/template-path'
import * as PP from '../../../core/shared/property-path'
import { betterReactMemo } from '../../../uuiui-deps'
import { jsxAttributesToProps } from '../../../core/shared/jsx-attributes'
import { getUtopiaIDFromJSXElement } from '../../../core/shared/uid-utils'
import utils from '../../../utils/utils'
import { PathForResizeContent, ResizesContentProp } from '../../../core/model/scene-utils'
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
  templatePath: InstancePath,
  componentName: string | null,
  props: SceneProps,
  children: Array<InstancePath>,
): void {
  const shouldIncludeCanvasRootInTheSpy = useContextSelector(
    RerenderUtopiaContext,
    (c) => c.shouldIncludeCanvasRootInTheSpy,
  )
  let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)

  const resizesContent = Boolean(utils.path(PP.getElements(PathForResizeContent), props) ?? false)

  if (shouldIncludeCanvasRootInTheSpy) {
    let capturedProps: {
      [ResizesContentProp]?: boolean
      style?: React.CSSProperties
      'data-uid'?: string
    } = {}
    if (resizesContent) {
      capturedProps[ResizesContentProp] = true
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
      children: children,
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
  validPaths: Array<InstancePath>
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
    const sceneUID = getUtopiaIDFromJSXElement(props.sceneElement)

    if (parentPath == null) {
      throw new Error(`Utopia Error: no parent template path provided for Scene (uid: ${sceneUID})`)
    }

    const sceneProps: SceneProps = React.useMemo(
      () => jsxAttributesToProps(inScope, props.sceneElement.props, requireResult),
      [inScope, props.sceneElement.props, requireResult],
    )
    const templatePath = React.useMemo(() => TP.appendToPath(parentPath, sceneUID), [
      parentPath,
      sceneUID,
    ])
    const scenePath = TP.scenePathForElementAtInstancePath(templatePath)

    const childrenWithPaths = props.sceneElement.children.map((child) => {
      const uid = getUtopiaID(child)
      const path = TP.appendToPath(templatePath, uid)
      return {
        child: child,
        path: path,
        uid: uid,
      }
    })

    const topLevelElementName = getTopLevelElementName(sceneProps.component)

    const validPaths = props.validPaths.concat(
      useGetValidTemplatePaths(topLevelElementName, scenePath),
    )

    useRunSpy(
      templatePath,
      topLevelElementName,
      sceneProps,
      childrenWithPaths.map(({ path }) => path),
    )

    let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)
    const hiddenInstances = useContextSelector(RerenderUtopiaContext, (c) => c.hiddenInstances)
    const shouldIncludeCanvasRootInTheSpy = useContextSelector(
      RerenderUtopiaContext,
      (c) => c.shouldIncludeCanvasRootInTheSpy,
    )

    const renderedChildren =
      childrenWithPaths.length == 0
        ? null
        : childrenWithPaths.map(({ child, path, uid }) => {
            return renderCoreElement(
              child,
              path,
              inScope,
              inScope,
              {
                [UTOPIA_UIDS_KEY]: uid,
              },
              requireResult,
              hiddenInstances,
              mutableUtopiaContext.fileBlobs,
              validPaths,
              undefined,
              undefined,
              metadataContext,
              mutableUtopiaContext.jsxFactoryFunctionName,
              null, // TODO Code error
              shouldIncludeCanvasRootInTheSpy,
              props.filePath,
            )
          })

    const propsWithScenePath = {
      ...sceneProps.props,
      [UTOPIA_SCENE_PATH]: scenePath,
      [UTOPIA_UIDS_KEY]: sceneUID,
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
          {renderedChildren}
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
