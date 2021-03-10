import * as React from 'react'
import { MapLike } from 'typescript'
import { EmptyScenePathForStoryboard } from '../../../core/model/scene-utils'
import { right } from '../../../core/shared/either'
import {
  ElementInstanceMetadata,
  emptyAttributeMetadatada,
  emptyComputedStyle,
  emptySpecialSizeMeasurements,
  getJSXElementNameNoPathName,
  JSXElement,
} from '../../../core/shared/element-template'
import {
  ElementPath,
  InstancePath,
  ScenePath,
  TemplatePath,
} from '../../../core/shared/project-file-types'
import { makeCanvasElementPropsSafe } from '../../../utils/canvas-react-utils'
import { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import * as TP from '../../../core/shared/template-path'
import { renderComponentUsingJsxFactoryFunction } from './ui-jsx-canvas-element-renderer-utils'
import { getTopLevelElementName, useGetValidTemplatePaths } from './scene-root'
import { UTOPIA_UID_KEY } from '../../../core/model/utopia-constants'
import { getUtopiaID } from '../../../core/model/element-template-utils'
import { ComponentRendererComponent } from './ui-jsx-canvas-component-renderer'
import { dropLast, last } from '../../../core/shared/array-utils'

export function buildSpyWrappedElement(
  jsx: JSXElement,
  finalProps: any,
  templatePath: InstancePath,
  extendedTemplatePath: ElementPath,
  metadataContext: UiJsxCanvasContextData,
  childrenTemplatePaths: Array<InstancePath>,
  childrenElements: Array<React.ReactNode>,
  Element: any,
  inScope: MapLike<any>,
  jsxFactoryFunctionName: string | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
  focusedElementTemplatePath: ElementPath | null,
  validPathsForCurrentScene: Array<InstancePath>,
): React.ReactElement {
  let props = {
    ...finalProps,
    key: TP.toComponentId(templatePath),
  }

  let topLevelElementName = ''

  let scenePath: ScenePath | null = null

  let fixedChildrenTemplatePaths: Array<InstancePath> = childrenTemplatePaths

  // focused app/card/card-root/button/button-root
  // storyboard-uid/scene-uid/app/card/card-root
  // storyboard-uid/scene-uid:app/card

  if (focusedElementTemplatePath != null) {
    if (TP.elementPathsEqual(extendedTemplatePath, focusedElementTemplatePath)) {
      // I am being spied
      // Replace the instance's UID with the definition's
      const instanceUID = getUtopiaID(jsx)
      const originalComponent = inScope[jsx.name.baseVariable]
      const originalUID = (originalComponent as ComponentRendererComponent)?.originalUID
      props[UTOPIA_UID_KEY] = originalUID ?? instanceUID

      // Add the root of this a child template path
      if (originalUID != null) {
        fixedChildrenTemplatePaths = [
          TP.instancePath(TP.scenePath(focusedElementTemplatePath), [originalUID]),
          ...childrenTemplatePaths,
        ]
      }
    } else if (
      // storyboard-uid/scene-uid/app-root/designated-scene/card-instance/card-root/button-instance - focus
      // storyboard-uid/scene-uid/app-root/designated-scene/card-instance - extended
      // storyboard-uid/scene-uid:app-root/designated-scene/card-instance - TP
      TP.elementIsDescendent(focusedElementTemplatePath, extendedTemplatePath) &&
      // last(templatePath.element) !== last(extendedTemplatePath) &&
      templatePath.element.length > 0
    ) {
      const pathElementToAdd = focusedElementTemplatePath[extendedTemplatePath.length]
      if (!childrenTemplatePaths.some((p) => TP.toUid(p) === pathElementToAdd)) {
        fixedChildrenTemplatePaths = [
          TP.appendToPath(templatePath, pathElementToAdd),
          ...childrenTemplatePaths,
        ]
      }
    }
  }

  if (
    focusedElementTemplatePath != null &&
    TP.elementPathsEqual(extendedTemplatePath, dropLast(focusedElementTemplatePath))
  ) {
    // I am rendering the parent of whatever's being spied

    scenePath = TP.scenePath(focusedElementTemplatePath)

    if (childrenTemplatePaths.length === 0) {
      topLevelElementName = (childrenElements[0] as React.ReactElement)?.props?.elementToRender
        ?.topLevelElementName // THIS IS A SPIKE, RELAX
      // const originalUid = (childrenElements[0] as React.ReactElement)?.props?.elementToRender
      //   ?.originalUID
      // fixedChildrenTemplatePaths = [TP.instancePath(scenePath, [originalUid])]
    } else {
      fixedChildrenTemplatePaths = childrenTemplatePaths.map((childTemplatePath, index) => {
        const originalUid = (childrenElements[index] as React.ReactElement)?.props?.elementToRender
          ?.originalUID

        if (
          TP.elementPathsEqual(extendedTemplatePath, dropLast(focusedElementTemplatePath ?? []))
        ) {
          // ERROR: this is not correct, because if you drilling into an element inside a component inside a component, that will totally TOTALLY not show up in childrenTemplatePaths
          // FFFFFUUUUU
          // we probably need to move data-utopia-valid-paths onto the spied element itself, instead of designating its parent as a "scene"

          topLevelElementName = (childrenElements[index] as React.ReactElement)?.props
            ?.elementToRender?.topLevelElementName // THIS IS A SPIKE, RELAX

          //   const fixedTemplatePath = TP.instancePath(scenePath!, [originalUid])

          //   return fixedTemplatePath
          // } else {
        }
        return childTemplatePath
        // }
      })
    }
  }

  const childrenElementsOrNull = childrenElements.length > 0 ? childrenElements : null
  const spyCallback = (reportedProps: any) => {
    const uid = getUtopiaID(jsx)
    const isInstance = TP.toUid(templatePath) === uid
    // Commented out because this is _actually_ a fix but we don't want to mix it in yet
    const shiftedTemplatePath = isInstance ? templatePath : TP.appendToPath(templatePath, uid)
    const instanceMetadata: ElementInstanceMetadata = {
      element: right(jsx),
      templatePath: shiftedTemplatePath,
      props: makeCanvasElementPropsSafe(reportedProps),
      globalFrame: null,
      localFrame: null,
      children: fixedChildrenTemplatePaths,
      componentInstance: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements, // This is not the nicest, but the results from the DOM walker will override this anyways
      computedStyle: emptyComputedStyle,
      attributeMetadatada: emptyAttributeMetadatada,
    }
    const isChildOfRootScene = TP.pathsEqual(
      TP.scenePathForPath(shiftedTemplatePath),
      EmptyScenePathForStoryboard,
    )
    if (!isChildOfRootScene || shouldIncludeCanvasRootInTheSpy) {
      metadataContext.current.spyValues.metadata[
        TP.toComponentId(shiftedTemplatePath)
      ] = instanceMetadata
    }
  }

  const spyWrapperProps: SpyWrapperProps = {
    elementToRender: Element,
    spyCallback: spyCallback,
    inScope: inScope,
    jsxFactoryFunctionName: jsxFactoryFunctionName,
    scenePath: scenePath,
    topLevelElementName: topLevelElementName,
    validPathsForCurrentScene: validPathsForCurrentScene,
  }

  return renderComponentUsingJsxFactoryFunction(
    inScope,
    jsxFactoryFunctionName,
    SpyWrapper,
    {
      ...props,
      ...spyWrapperProps,
    },
    childrenElementsOrNull,
  )
}

interface SpyWrapperProps {
  spyCallback: (finalProps: any) => void
  elementToRender: React.ComponentType<any>
  inScope: MapLike<any>
  jsxFactoryFunctionName: string | null
  topLevelElementName: string | null
  scenePath: ScenePath | null
  validPathsForCurrentScene: Array<InstancePath>
}
const SpyWrapper: React.FunctionComponent<SpyWrapperProps> = (props) => {
  const {
    spyCallback,
    elementToRender: ElementToRender,
    inScope,
    jsxFactoryFunctionName,
    topLevelElementName,
    scenePath,
    validPathsForCurrentScene,
    ...passThroughProps
  } = props
  const validPaths = useGetValidTemplatePaths(topLevelElementName, scenePath ?? TP.scenePath([]))

  const cumulativeValidPathsAsString = [...validPathsForCurrentScene, ...validPaths]
    .map(TP.toString)
    .join(' ')

  spyCallback(passThroughProps)
  const result = renderComponentUsingJsxFactoryFunction(
    inScope,
    jsxFactoryFunctionName,
    ElementToRender,
    passThroughProps,
  )
  if (scenePath == null) {
    return result
  } else {
    // this element is promoted to be a temporary Scene
    return React.cloneElement(result, {
      'data-utopia-valid-paths': cumulativeValidPathsAsString,
      'data-utopia-scene-id': TP.elementPathToString(scenePath?.sceneElementPath),
    })
  }
}
SpyWrapper.displayName = 'SpyWrapper'
