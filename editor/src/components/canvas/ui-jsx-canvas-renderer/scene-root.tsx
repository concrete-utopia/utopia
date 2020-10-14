import * as React from 'react'
import { MapLike } from 'typescript'
import { NormalisedFrame, View } from 'utopia-api'
import { getValidTemplatePaths } from '../../../core/model/element-template-utils'
import { left } from '../../../core/shared/either'
import {
  emptySpecialSizeMeasurements,
  emptyComputedStyle,
} from '../../../core/shared/element-template'
import { InstancePath, TemplatePath, SceneContainer } from '../../../core/shared/project-file-types'
import { colorTheme, UtopiaStyles } from '../../../uuiui'
import { UIFileBase64Blobs } from '../../editor/store/editor-state'
import { UiJsxCanvasContextData, UiJsxCanvasContext } from '../ui-jsx-canvas'
import { ComponentRendererComponent } from './ui-jsx-canvas-component-renderer'
import { RerenderUtopiaContext, SceneLevelUtopiaContext } from './ui-jsx-canvas-contexts'
import { renderComponentUsingJsxFactoryFunction } from './ui-jsx-canvas-element-renderer-utils'
import * as TP from '../../../core/shared/template-path'

interface SceneRootProps {
  content: ComponentRendererComponent | undefined
  templatePath: InstancePath
  requireResult: MapLike<any>
  inScope: MapLike<any>
  hiddenInstances: Array<TemplatePath>
  componentProps: MapLike<any>
  style: React.CSSProperties
  jsxFactoryFunctionName: string | null
  container: SceneContainer
  component: string | null
  sceneResizesContent: boolean

  // this is even worse: this is secret props that are passed down from a utopia parent View
  // we put this here in case the Scene is inside another View
  parentAbsoluteFrame?: NormalisedFrame
  fileBlobs: UIFileBase64Blobs

  sceneUID: string
  sceneLabel: string | undefined

  'data-uid'?: string // the data uid
}

export const SceneRoot: React.FunctionComponent<SceneRootProps> = (props) => {
  const {
    content,
    templatePath,
    requireResult,
    inScope,
    hiddenInstances,
    fileBlobs,
    componentProps,
    style,
    container,
    jsxFactoryFunctionName,
    component,
    sceneResizesContent,
    sceneUID,
    'data-uid': dataUidIgnore,
    ...inputProps
  } = props

  const scenePath = TP.scenePath(TP.elementPathForPath(templatePath))

  const rerenderUtopiaContext = React.useContext(RerenderUtopiaContext)
  let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)

  metadataContext.current.spyValues.scenes[TP.toString(scenePath)] = {
    scenePath: scenePath,
    templatePath: templatePath,
    container: container,
    component: component,
    sceneResizesContent: sceneResizesContent,
    globalFrame: null, // the real frame comes from the DOM walker
    label: props.sceneLabel,
    style: style,
  }
  if (rerenderUtopiaContext.shouldIncludeCanvasRootInTheSpy) {
    metadataContext.current.spyValues.metadata[TP.toComponentId(templatePath)] = {
      element: left('Scene'),
      templatePath: templatePath,
      props: {},
      globalFrame: null,
      localFrame: null,
      childrenTemplatePaths: [],
      componentInstance: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements, // This is not the nicest, but the results from the DOM walker will override this anyways
      computedStyle: emptyComputedStyle,
    }
  }

  let rootElement = null
  let validPaths: Array<InstancePath> = []
  if (content != null) {
    const passthroughProps = {
      ...inputProps,
      ...componentProps,
    }

    rootElement = renderComponentUsingJsxFactoryFunction(
      inScope,
      jsxFactoryFunctionName,
      content,
      passthroughProps,
      undefined,
    )

    const utopiaJsxComponent = rerenderUtopiaContext.topLevelElements.get(
      content.topLevelElementName,
    )
    if (utopiaJsxComponent != null) {
      validPaths = getValidTemplatePaths(utopiaJsxComponent, scenePath)
    }
  }

  const sceneStyle: React.CSSProperties = {
    position: 'relative',
    backgroundColor: colorTheme.emphasizedBackground.value,
    boxShadow: rerenderUtopiaContext.canvasIsLive
      ? UtopiaStyles.scene.live.boxShadow
      : UtopiaStyles.scene.editing.boxShadow,
    ...style,
  }

  return (
    <SceneLevelUtopiaContext.Provider value={{ validPaths: validPaths, scenePath: scenePath }}>
      <View
        data-utopia-scene-id={TP.toString(scenePath)}
        data-utopia-valid-paths={validPaths.map(TP.toString).join(' ')}
        style={sceneStyle}
        layout={{
          ...container,
        }}
      >
        {rootElement}
      </View>
    </SceneLevelUtopiaContext.Provider>
  )
}
SceneRoot.displayName = 'SceneRoot'
