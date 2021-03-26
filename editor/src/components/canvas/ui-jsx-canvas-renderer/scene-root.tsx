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
  isJSXElement,
  getJSXElementNameAsString,
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
import { PathForResizeContent } from '../../../core/model/scene-utils'
import { UTOPIA_SCENE_PATH, UTOPIA_UIDS_KEY } from '../../../core/model/utopia-constants'
import { optionalMap } from '../../../core/shared/optional-utils'
import { flattenArray, mapDropNulls } from '../../../core/shared/array-utils'
import { useEditorState } from '../../editor/store/store-hook'
import { getFileForName, getOpenUIJSFileKey } from '../../editor/store/editor-state'
import { fastForEach } from '../../../core/shared/utils'
import { getTopLevelElements, useGetTopLevelElements } from './ui-jsx-canvas-top-level-elements'

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
    metadataContext.current.spyValues.metadata[TP.toComponentId(templatePath)] = {
      element: left('Scene'),
      templatePath: templatePath,
      props: {},
      globalFrame: null,
      localFrame: null,
      children: [],
      componentInstance: false,
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements,
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
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

  const topLevelElements = useGetTopLevelElements(uiFilePath)
  let topLevelJSXComponents: Map<string, UtopiaJSXComponent> = new Map()
  fastForEach(topLevelElements, (topLevelElement) => {
    if (isUtopiaJSXComponent(topLevelElement)) {
      topLevelJSXComponents.set(topLevelElement.name, topLevelElement)
    }
  })

  const focusedElementPath = useContextSelector(RerenderUtopiaContext, (c) => c.focusedElementPath)

  return getValidTemplatePaths(
    topLevelJSXComponents,
    focusedElementPath,
    topLevelElementName,
    TP.dynamicPathToStaticPath(scenePath),
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

    const childrenWithPathsAndNames = props.sceneElement.children.map((child) => {
      const uid = getUtopiaID(child)
      const instance = TP.appendToPath(templatePath, uid)
      const scene = TP.scenePathForElementAtInstancePath(instance)
      return {
        child: child,
        instance: instance,
        scene: scene,
        uid: uid,
        name: isJSXElement(child) ? getJSXElementNameAsString(child.name) : null,
      }
    })

    const jsxElementChildren = mapDropNulls(
      ({ instance, scene, name }) =>
        name == null ? null : { scene: scene, name: name, instance: instance },
      childrenWithPathsAndNames,
    )

    const uiFilePath = useContextSelector(
      UtopiaProjectContext,
      (c) => c.openStoryboardFilePathKILLME,
    )
    const topLevelElements = useGetTopLevelElements(uiFilePath)
    let topLevelJSXComponents: Map<string, UtopiaJSXComponent> = new Map()
    fastForEach(topLevelElements, (topLevelElement) => {
      if (isUtopiaJSXComponent(topLevelElement)) {
        topLevelJSXComponents.set(topLevelElement.name, topLevelElement)
      }
    })
    const focusedElementPath = useContextSelector(
      RerenderUtopiaContext,
      (c) => c.focusedElementPath,
    )

    const unflattenedValidPaths = jsxElementChildren.map(({ name, scene }) =>
      getValidTemplatePaths(topLevelJSXComponents, focusedElementPath, name, scene),
    )
    const validPaths = [
      templatePath,
      ...jsxElementChildren.map(({ instance }) => instance),
      ...flattenArray(unflattenedValidPaths),
    ]

    // const baseValidPaths = useGetValidTemplatePaths(
    //   topLevelElements[0].name,
    //   topLevelElements[0].scene,
    // )
    // const validPaths = baseValidPaths
    // .concat(
    //   childrenWithPathsAndNames.map(({ instance }) => instance),
    // )

    let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)
    const hiddenInstances = useContextSelector(RerenderUtopiaContext, (c) => c.hiddenInstances)

    const shouldIncludeCanvasRootInTheSpy = useContextSelector(
      RerenderUtopiaContext,
      (c) => c.shouldIncludeCanvasRootInTheSpy,
    )

    const sceneResizesContent = Boolean(
      utils.path(PP.getElements(PathForResizeContent), props) ?? false,
    )

    // useRunSpy(scenePath, templatePath, topLevelElements[0].name, sceneProps)
    // useRunSpy(scenePath, topLevelElements[0].instance, topLevelElements[0].name, sceneProps)

    // Add metadata for the actual scene
    if (shouldIncludeCanvasRootInTheSpy) {
      metadataContext.current.spyValues.metadata[TP.toComponentId(templatePath)] = {
        element: left('Scene'),
        templatePath: templatePath,
        props: {},
        globalFrame: null,
        localFrame: null,
        children: childrenWithPathsAndNames.map(({ instance }) => instance),
        // children: [],
        componentInstance: false,
        isEmotionOrStyledComponent: false,
        specialSizeMeasurements: emptySpecialSizeMeasurements,
        computedStyle: emptyComputedStyle,
        attributeMetadatada: emptyAttributeMetadatada,
      }
    }

    metadataContext.current.spyValues.scenes[TP.toString(scenePath)] = {
      scenePath: scenePath,
      templatePath: templatePath,
      component: childrenWithPathsAndNames[0]?.name,
      sceneResizesContent: sceneResizesContent,
      globalFrame: null,
      label: (sceneProps.props ?? {})['data-label'],
      style: sceneProps.props?.style ?? {},
    }

    const renderedChildren = childrenWithPathsAndNames.map(
      ({ name, child, instance, scene, uid }, index) => {
        const propsWithScenePath = {
          ...sceneProps.props,
          [UTOPIA_SCENE_PATH]: scene,
          [UTOPIA_UIDS_KEY]: uid,
        }

        if (shouldIncludeCanvasRootInTheSpy) {
          metadataContext.current.spyValues.metadata[TP.toComponentId(instance)] = {
            element: left('Scene'),
            templatePath: instance,
            props: {},
            globalFrame: null,
            localFrame: null,
            children: [],
            componentInstance: false,
            isEmotionOrStyledComponent: false,
            specialSizeMeasurements: emptySpecialSizeMeasurements,
            computedStyle: emptyComputedStyle,
            attributeMetadatada: emptyAttributeMetadatada,
          }
        }

        metadataContext.current.spyValues.scenes[TP.toString(scene)] = {
          scenePath: scene,
          templatePath: instance,
          component: name,
          sceneResizesContent: sceneResizesContent,
          globalFrame: null,
          label: propsWithScenePath['data-label'],
          style: propsWithScenePath.style ?? {},
        }

        return renderCoreElement(
          child,
          instance,
          inScope,
          inScope,
          propsWithScenePath,
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
          uiFilePath!,
        )
      },
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
          data-uid={sceneUID}
          data-utopia-scene-id={TP.toString(scenePath)}
          data-utopia-valid-paths={validPaths.map(TP.toString).join(' ')}
          style={sceneStyle}
        >
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
