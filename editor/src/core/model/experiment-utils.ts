import * as TP from '../../core/shared/template-path'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { isRight } from '../shared/either'
import {
  getJSXElementNameAsString,
  isJSXElement,
  JSXElement,
  JSXMetadata,
  UtopiaJSXComponent,
} from '../shared/element-template'
import { InstancePath, ScenePath, TemplatePath } from '../shared/project-file-types'
import { MetadataUtils } from './element-metadata-utils'
import { UtopiaJSXComponentsByName } from './project-file-utils'

interface MatchingComponentResult {
  element: JSXElement
  component: UtopiaJSXComponent
}

export function getMatchingComponentForInstancePath(
  path: InstancePath,
  metadata: JSXMetadata,
  componentsByName: UtopiaJSXComponentsByName,
): MatchingComponentResult | null {
  const elementMetadata = MetadataUtils.getElementByInstancePathMaybe(metadata.elements, path)
  if (elementMetadata != null) {
    const element = elementMetadata.element
    if (isRight(element) && isJSXElement(element.value)) {
      const elementName = getJSXElementNameAsString(element.value.name)
      const component = componentsByName[elementName]
      return component == null
        ? null
        : {
            element: element.value,
            component: component,
          }
    }
  }

  return null
}

export function getMatchingComponentForScenePath(
  path: ScenePath,
  metadata: JSXMetadata,
  componentsByName: UtopiaJSXComponentsByName,
): MatchingComponentResult | null {
  const sceneMetadata = MetadataUtils.findSceneByTemplatePath(metadata.components, path)
  const componentName = sceneMetadata?.component
  const maybeMatchingComponent = componentName == null ? null : componentsByName[componentName]
  const maybeMatchingRootElement: JSXElement | null =
    maybeMatchingComponent != null && isJSXElement(maybeMatchingComponent.rootElement)
      ? maybeMatchingComponent.rootElement
      : null
  return maybeMatchingComponent == null || maybeMatchingRootElement == null
    ? null
    : {
        element: maybeMatchingRootElement,
        component: maybeMatchingComponent,
      }
}

export function findNearestComponentAncestor(
  path: TemplatePath,
  metadata: JSXMetadata,
  componentsByName: UtopiaJSXComponentsByName,
): MatchingComponentResult | null {
  if (TP.isInstancePath(path)) {
    const maybeMatchingComponent = getMatchingComponentForInstancePath(
      path,
      metadata,
      componentsByName,
    )
    return (
      maybeMatchingComponent ??
      findNearestComponentAncestor(TP.parentPath(path), metadata, componentsByName)
    )
  } else {
    return getMatchingComponentForScenePath(path, metadata, componentsByName)
  }
}

export function findMatchingComponent(
  path: TemplatePath,
  metadata: JSXMetadata,
  componentsByName: UtopiaJSXComponentsByName,
  useNearestAncestor: boolean,
): UtopiaJSXComponent | null {
  if (useNearestAncestor) {
    return findNearestComponentAncestor(path, metadata, componentsByName)?.component ?? null
  } else if (TP.isInstancePath(path)) {
    return getMatchingComponentForInstancePath(path, metadata, componentsByName)?.component ?? null
  } else {
    return getMatchingComponentForScenePath(path, metadata, componentsByName)?.component ?? null
  }
}

export function filterScenesWithElementPredicate(
  paths: TemplatePath[],
  elementPredicate: (elem: string) => boolean,
): TemplatePath[] {
  function scenePathSatisfiesElementPredicate(path: ScenePath): boolean {
    return path.sceneElementPath.every(elementPredicate)
  }

  return paths.filter((path) =>
    TP.isInstancePath(path)
      ? scenePathSatisfiesElementPredicate(path.scene)
      : scenePathSatisfiesElementPredicate(path),
  )
}
