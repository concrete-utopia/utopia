import { MapLike } from 'typescript'
import {
  isJSXElement,
  isUtopiaJSXComponent,
  JSXElementChild,
  TopLevelElement,
  UtopiaJSXComponent,
} from '../../shared/element-template'
import { optionalMap } from '../../shared/optional-utils'
import { isParseSuccess, ParsedTextFile, StaticElementPath } from '../../shared/project-file-types'
import * as TP from '../../shared/template-path'
import { setUtopiaIDOnJSXElement } from '../../shared/uid-utils'
import { getUtopiaID, transformJSXComponentAtElementPath } from '../../model/element-template-utils'
import { getComponentsFromTopLevelElements } from '../../model/project-file-utils'
import { mapArrayToDictionary } from '../../shared/array-utils'
import { emptySet } from '../../shared/set-utils'
import { fastForEach } from '../../shared/utils'

// TODO move this function from editor-sate to a shared utils file, delete this copy-paste
export function applyUtopiaJSXComponentsChanges(
  topLevelElements: Array<TopLevelElement>,
  newUtopiaComponents: Array<UtopiaJSXComponent>,
): Array<TopLevelElement> {
  // Run through the old top level elements, replacing the exported elements with those in the
  // newly updated result with the same name.
  // If it doesn't exist in the updated result, delete it.
  // For any new items in the updated result, add them in.
  const addedSoFar: Set<string> = emptySet()
  let newTopLevelElements: Array<TopLevelElement> = []
  fastForEach(topLevelElements, (oldTopLevelElement) => {
    if (isUtopiaJSXComponent(oldTopLevelElement)) {
      const updatedElement = newUtopiaComponents.find((e) => e.name === oldTopLevelElement.name)
      if (updatedElement !== undefined) {
        addedSoFar.add(updatedElement.name)
        newTopLevelElements.push(updatedElement)
      }
    } else {
      newTopLevelElements.push(oldTopLevelElement)
    }
  })

  fastForEach(newUtopiaComponents, (updatedElement) => {
    if (!addedSoFar.has(updatedElement.name)) {
      newTopLevelElements.push(updatedElement)
    }
  })

  return newTopLevelElements
}

export function fixParseSuccessUIDs(
  oldParsed: ParsedTextFile | null,
  newParsed: ParsedTextFile,
): ParsedTextFile {
  if (oldParsed == null || !isParseSuccess(oldParsed) || !isParseSuccess(newParsed)) {
    // we won't try to fix parse failures
    return newParsed
  }

  let newToOldUidMapping: {
    [newUID: string]: {
      oldUID: string
      newUID: string
      pathToModify: StaticElementPath
    }
  } = {}

  zipProjectContents(
    oldParsed.topLevelElements,
    newParsed.topLevelElements,
    (oldElement, newElement, newPath) => {
      const oldUID = optionalMap(getUtopiaID, oldElement)
      const newUID = optionalMap(getUtopiaID, newElement)

      if (oldUID != null && newUID != null && oldUID !== newUID) {
        // we have a UID mismatch
        newToOldUidMapping[newUID] = { oldUID: oldUID, newUID: newUID, pathToModify: newPath }
      }
    },
  )

  const newToOldUidMappingArray = Object.values(newToOldUidMapping)

  if (newToOldUidMappingArray.length !== 1) {
    // we found no uid mismatch or we found too many mismatched UIDs and so we bail out
    return newParsed
  } else {
    // we found a single UID mismatch, which means there's a very good chance that it was an update element, let's fix that up
    let workingComponents = getComponentsFromTopLevelElements(newParsed.topLevelElements)

    newToOldUidMappingArray.forEach((mapping) => {
      workingComponents = transformJSXComponentAtElementPath(
        workingComponents,
        mapping.pathToModify,
        (element) => {
          return setUtopiaIDOnJSXElement(element, mapping.oldUID)
        },
      )
    })

    const fixedTopLevelElements = applyUtopiaJSXComponentsChanges(
      newParsed.topLevelElements,
      workingComponents,
    )

    const fixedHighlightBounds = mapArrayToDictionary(
      Object.values(newParsed.highlightBounds),
      (highlightBound) => {
        const uidFix = newToOldUidMapping[highlightBound.uid]
        if (uidFix != null) {
          return uidFix.oldUID
        } else {
          return highlightBound.uid
        }
      },
      (highlightBound) => {
        const uidFix = newToOldUidMapping[highlightBound.uid]
        if (uidFix != null) {
          return {
            ...highlightBound,
            uid: uidFix.oldUID,
          }
        } else {
          return highlightBound
        }
      },
    )

    return {
      ...newParsed,
      topLevelElements: fixedTopLevelElements,
      highlightBounds: fixedHighlightBounds,
    }
  }
}

function zipProjectContents(
  firstTopLevelElements: Array<TopLevelElement>,
  secondTopLevelElements: Array<TopLevelElement>,
  onElement: (
    firstContents: JSXElementChild | null,
    secondContents: JSXElementChild,
    newTemplatePath: StaticElementPath,
  ) => void,
): void {
  secondTopLevelElements.forEach((newTopLevelElement, index) => {
    if (isUtopiaJSXComponent(newTopLevelElement)) {
      const uid = getUtopiaID(newTopLevelElement.rootElement)

      const oldTopLevelElement = firstTopLevelElements.find((tle): tle is UtopiaJSXComponent => {
        return isUtopiaJSXComponent(tle) && getUtopiaID(tle.rootElement) === uid
      })

      if (oldTopLevelElement != null) {
        walkElementChildren(
          TP.emptyElementPath,
          [oldTopLevelElement.rootElement],
          [newTopLevelElement.rootElement],
          onElement,
        )
      }
    }
  })
}

function walkElementChildren(
  pathSoFar: StaticElementPath,
  oldElements: Array<JSXElementChild>,
  newElements: Array<JSXElementChild>,
  onElement: (
    firstContents: JSXElementChild | null,
    secondContents: JSXElementChild,
    newTemplatePath: StaticElementPath,
  ) => void,
): void {
  /**
   * this first version works by trying to match up indexes. this is really primitive.
   * here's some ideas how could we improve it
   * • before correcting a UID, make sure the uids of the preceding and proceding siblings are matching! this makes the comparison more robust and makes us less exposed to off-by-one errors
   * • try to match offsets: if the user deletes or inserts an element, all subsequent uids will be shifted and thus mismatched in an index-to-index comparison
   */

  newElements.forEach((newElement, index) => {
    const oldElement: JSXElementChild | null = oldElements[index]
    const newUid = getUtopiaID(newElement)
    const path = TP.appendToElementPath(pathSoFar, newUid)

    onElement(oldElement, newElement, path)

    if (
      oldElement != null &&
      newElement != null &&
      isJSXElement(oldElement) &&
      isJSXElement(newElement)
    ) {
      walkElementChildren(path, oldElement.children, newElement.children, onElement)
    }
  })
}
