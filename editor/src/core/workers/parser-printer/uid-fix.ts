import {
  ElementsWithin,
  isJSXArbitraryBlock,
  isJSXElement,
  isJSXFragment,
  isUtopiaJSXComponent,
  JSXElementChild,
  TopLevelElement,
  UtopiaJSXComponent,
} from '../../shared/element-template'
import {
  isParseSuccess,
  ParsedTextFile,
  ParseSuccess,
  StaticElementPathPart,
} from '../../shared/project-file-types'
import * as EP from '../../shared/element-path'
import { setUtopiaIDOnJSXElement } from '../../shared/uid-utils'
import {
  findJSXElementChildAtPath,
  getUtopiaID,
  transformJSXComponentAtElementPath,
} from '../../model/element-template-utils'
import {
  applyUtopiaJSXComponentsChanges,
  getComponentsFromTopLevelElements,
} from '../../model/project-file-utils'
import { mapArrayToDictionary } from '../../shared/array-utils'
import { fastForEach } from '../../shared/utils'

export function fixParseSuccessUIDs(
  oldParsed: ParseSuccess | null,
  newParsed: ParsedTextFile,
  alreadyExistingUIDs: Set<string>,
): ParsedTextFile {
  if (oldParsed == null || !isParseSuccess(newParsed)) {
    // we won't try to fix parse failures
    return newParsed
  }

  let newToOldUidMapping: {
    [newUID: string]: {
      oldUID: string
      newUID: string
      pathToModify: StaticElementPathPart
      oldPathToRestore: StaticElementPathPart
    }
  } = {}

  zipTopLevelElements(
    oldParsed.topLevelElements,
    newParsed.topLevelElements,
    (
      oldUID: string,
      newUID: string,
      oldPathToRestore: StaticElementPathPart,
      newPath: StaticElementPathPart,
    ) => {
      if (oldUID !== newUID && !alreadyExistingUIDs.has(oldUID)) {
        // we have a UID mismatch and have confirmed that UID doesn't exist elsewhere
        newToOldUidMapping[newUID] = {
          oldUID: oldUID,
          newUID: newUID,
          pathToModify: newPath,
          oldPathToRestore: oldPathToRestore,
        }
      }
    },
  )

  const newToOldUidMappingArray = Object.values(newToOldUidMapping)

  if (newToOldUidMappingArray.length === 1) {
    // we found a single UID mismatch, which means there's a very good chance that it was an update element, let's fix that up
    let workingComponents = getComponentsFromTopLevelElements(newParsed.topLevelElements)

    newToOldUidMappingArray.forEach((mapping) => {
      const oldPathAlreadyExistingElement = findJSXElementChildAtPath(
        workingComponents,
        EP.elementPath([mapping.oldPathToRestore]),
      )

      if (oldPathAlreadyExistingElement == null) {
        workingComponents = transformJSXComponentAtElementPath(
          workingComponents,
          mapping.pathToModify,
          (element) => {
            return setUtopiaIDOnJSXElement(element, mapping.oldUID)
          },
          isJSXElement,
        )
      } else {
        // this is awkward, there is already an element with this UID. it means we need to bail out from this update
        return
      }
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
  } else {
    // we found no uid mismatch or we found too many mismatched UIDs and so we bail out
    return newParsed
  }
}

type OnElement = (
  oldUID: string,
  newUID: string,
  oldPathToRestore: StaticElementPathPart,
  newElementPath: StaticElementPathPart,
) => void

function zipTopLevelElements(
  firstTopLevelElements: Array<TopLevelElement>,
  secondTopLevelElements: Array<TopLevelElement>,
  onElement: OnElement,
): void {
  const firstComponents = getComponentsFromTopLevelElements(firstTopLevelElements)
  const secondComponents = getComponentsFromTopLevelElements(secondTopLevelElements)

  firstComponents.forEach((firstComponent, index) => {
    if (secondComponents.length > index) {
      const secondComponent = secondComponents[index]

      walkElementChildren(
        EP.emptyElementPathPart,
        [firstComponent.rootElement],
        [secondComponent.rootElement],
        onElement,
      )
    }
  })
}

function walkElementsWithin(
  pathSoFar: StaticElementPathPart,
  oldElements: ElementsWithin,
  newElements: ElementsWithin,
  onElement: OnElement,
): void {
  const oldElementKeys = Object.keys(oldElements)
  const newElementKeys = Object.keys(newElements)
  newElementKeys.forEach((elementKey, index) => {
    const newElement = newElements[elementKey]
    const oldElementKey: string | null = oldElementKeys[index] ?? null
    const oldElement: JSXElementChild | null =
      oldElementKey == null ? null : oldElements[oldElementKey] ?? null

    compareAndWalkElements(oldElement, newElement, pathSoFar, onElement)
  })
}

function walkElementChildren(
  pathSoFar: StaticElementPathPart,
  oldElements: Array<JSXElementChild>,
  newElements: Array<JSXElementChild>,
  onElement: OnElement,
): void {
  newElements.forEach((newElement, index) => {
    const oldElement: JSXElementChild | null = oldElements[index] ?? null

    compareAndWalkElements(oldElement, newElement, pathSoFar, onElement)
  })
}

function compareAndWalkElements(
  oldElement: JSXElementChild | null,
  newElement: JSXElementChild,
  pathSoFar: StaticElementPathPart,
  onElement: OnElement,
) {
  /**
   * this first version works by trying to match up indexes. this is really primitive.
   * here's some ideas how could we improve it
   * • before correcting a UID, make sure the uids of the preceding and proceding siblings are matching! this makes the comparison more robust and makes us less exposed to off-by-one errors
   * • try to match offsets: if the user deletes or inserts an element, all subsequent uids will be shifted and thus mismatched in an index-to-index comparison
   */

  if (oldElement != null) {
    if (isJSXElement(oldElement) && isJSXElement(newElement)) {
      const oldUID = getUtopiaID(oldElement)
      const newUid = getUtopiaID(newElement)
      const path = EP.appendToElementPath(pathSoFar, newUid)
      const oldPathToRestore = EP.appendToElementPath(pathSoFar, oldUID)
      onElement(oldUID, newUid, oldPathToRestore, path)
      walkElementChildren(path, oldElement.children, newElement.children, onElement)
    } else if (isJSXFragment(oldElement) && isJSXFragment(newElement)) {
      walkElementChildren(pathSoFar, oldElement.children, newElement.children, onElement)
    } else if (isJSXArbitraryBlock(oldElement) && isJSXArbitraryBlock(newElement)) {
      walkElementsWithin(pathSoFar, oldElement.elementsWithin, newElement.elementsWithin, onElement)
    }
  }
}
