import { type AllElementProps } from '../../components/editor/store/editor-state'
import { type ElementPath, type PropertyPath } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import * as EP from '../shared/element-path'
import * as ObjectPath from 'object-path'

export function getElementPropValue(
  allElementProps: AllElementProps,
  target: ElementPath,
  propPath: PropertyPath,
): unknown {
  const propsForElement = allElementProps[EP.toString(target)] ?? {}
  return ObjectPath.get(propsForElement, PP.getElements(propPath))
}

export function elementHasValueForProp(
  allElementProps: AllElementProps,
  target: ElementPath,
  propPath: PropertyPath,
): boolean {
  return getElementPropValue(allElementProps, target, propPath) != null
}
