import { last, stripNulls } from '../../core/shared/array-utils'
import { getDOMAttribute } from '../../core/shared/dom-utils'
import { WindowPoint } from '../../core/shared/math-utils'
import { TemplatePath } from '../../core/shared/project-file-types'
import * as TP from '../../core/shared/template-path'

// eslint-disable-next-line no-restricted-globals
export function findParentSceneValidPaths(target: Element): Array<string> | null {
  const validPaths = getDOMAttribute(target, 'data-utopia-valid-paths')
  if (validPaths != null) {
    return validPaths.split(' ')
  } else {
    if (target.parentElement != null) {
      return findParentSceneValidPaths(target.parentElement)
    } else {
      return null
    }
  }
}

export function findFirstParentWithValidUID(
  validTemplatePaths: Array<string>,
  // eslint-disable-next-line no-restricted-globals
  target: Element,
): string | null {
  const uid = getDOMAttribute(target, 'data-uid')
  const originalUid = getDOMAttribute(target, 'data-utopia-original-uid')
  if (originalUid != null && validTemplatePaths.find((tp) => tp.endsWith(originalUid))) {
    return last(validTemplatePaths.filter((tp) => tp.endsWith(originalUid))) ?? null
  } else if (uid != null && validTemplatePaths.find((tp) => tp.endsWith(uid))) {
    return last(validTemplatePaths.filter((tp) => tp.endsWith(uid))) ?? null
  } else {
    if (target.parentElement != null) {
      return findFirstParentWithValidUID(validTemplatePaths, target.parentElement)
    } else {
      return null
    }
  }
}

export function getAllTargetsAtPoint(point: WindowPoint | null): Array<TemplatePath> {
  if (point == null) {
    return []
  }
  const elementsUnderPoint = document.elementsFromPoint(point.x, point.y)
  return stripNulls(
    elementsUnderPoint.map((element) => {
      const validTPs = findParentSceneValidPaths(element)
      if (validTPs != null) {
        const foundValidtemplatePath = findFirstParentWithValidUID(validTPs, element)
        if (foundValidtemplatePath != null) {
          return TP.fromString(foundValidtemplatePath)
        }
      }
      return null
    }),
  )
}
