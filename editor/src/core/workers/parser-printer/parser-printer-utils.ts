import * as G from 'graphlib'
import { Graph } from 'graphlib'
import * as TS from 'typescript'
import { addToMapOfArraysUnique } from '../../shared/array-utils'
import { Either, left, right } from '../../shared/either'
import {
  isJSXArbitraryBlock,
  isJSXElement,
  isUtopiaJSXComponent,
  JSXAttribute,
  JSXAttributes,
  JSXElementChild,
  TopLevelElement,
  UtopiaJSXComponent,
  JSXElement,
  isJSXFragment,
} from '../../shared/element-template'
import { ErrorMessage } from '../../shared/error-messages'
import { defaultIfNull, forceNotNull } from '../../shared/optional-utils'
import {
  CanvasElementMetadataMap,
  ElementCanvasMetadata,
  ParseFailure,
} from '../../shared/project-file-types'
import { fixUtopiaElement, getUtopiaIDFromJSXElement } from '../../shared/uid-utils'
import { fastForEach } from '../../shared/utils'
import { parseFailure } from '../common/project-file-utils'
import { createCodeSnippetFromCode } from '../ts/ts-utils'
import { RawSourceMap } from '../ts/ts-typings/RawSourceMap'
import { SourceMapConsumer, SourceNode } from 'source-map'

export interface TopLevelElementAndCodeContext {
  element: TopLevelElement
  bounds: NodesBounds
}

// Checks if the first value is greater than the second one.
export function lineAndCharacterGreaterThan(
  first: TS.LineAndCharacter,
  second: TS.LineAndCharacter,
): boolean {
  if (first.line > second.line) {
    return true
  } else if (first.line === second.line) {
    return first.character > second.character
  } else {
    return false
  }
}

export interface NodesBounds {
  start: TS.LineAndCharacter
  end: TS.LineAndCharacter
}

export function getBoundsOfNodes(
  sourceFile: TS.SourceFile,
  node: TS.Node | Array<TS.Node>,
): NodesBounds {
  let workingStart: TS.LineAndCharacter | null = null as TS.LineAndCharacter | null
  let workingEnd: TS.LineAndCharacter | null = null as TS.LineAndCharacter | null

  let nodes: Array<TS.Node> = []
  if (Array.isArray(node)) {
    if (node.length === 0) {
      throw new Error('Cannot get bounds of empty node array.')
    }
    nodes = node
  } else {
    nodes = [node]
  }
  fastForEach(nodes, (n) => {
    const start = TS.getLineAndCharacterOfPosition(sourceFile, n.getStart(sourceFile))
    if (workingStart == null || lineAndCharacterGreaterThan(workingStart, start)) {
      workingStart = start
    }
    const end = TS.getLineAndCharacterOfPosition(sourceFile, n.end)
    if (workingEnd == null || lineAndCharacterGreaterThan(end, workingEnd)) {
      workingEnd = end
    }
  })
  if (workingStart == null || workingEnd == null) {
    throw new Error('Invalid state, bounds should exist.')
  } else {
    return {
      start: workingStart,
      end: workingEnd,
    }
  }
}

export function guaranteeUniqueUidsFromTopLevel(
  topLevelElements: Array<TopLevelElementAndCodeContext>,
): Array<TopLevelElementAndCodeContext> {
  return topLevelElements.map((tle) => {
    if (tle.element.type === 'UTOPIA_JSX_COMPONENT') {
      return {
        ...tle,
        element: {
          ...tle.element,
          rootElement: fixUtopiaElement(tle.element.rootElement, []),
        },
      }
    } else {
      return tle
    }
  })
}

export function attachMetadataToElements(
  topLevelElements: TopLevelElementAndCodeContext[],
  elementMetadataMap: CanvasElementMetadataMap,
): TopLevelElementAndCodeContext[] {
  function attachMetadataToElementsInner(element: JSXElementChild): JSXElementChild {
    if (isJSXElement(element)) {
      const fixedChildren = element.children.map(attachMetadataToElementsInner)
      let elementMetadata: ElementCanvasMetadata | null = null
      try {
        const elementUID = getUtopiaIDFromJSXElement(element)
        elementMetadata = defaultIfNull<ElementCanvasMetadata | null>(
          null,
          elementMetadataMap[elementUID],
        )
      } catch (e) {
        elementMetadata = null
      }

      return {
        ...element,
        metadata: elementMetadata,
        children: fixedChildren,
      }
    } else {
      return element
    }
  }

  return topLevelElements.map((tle) => {
    if (tle.element.type === 'UTOPIA_JSX_COMPONENT') {
      const utopiaComponent: UtopiaJSXComponent = tle.element
      return {
        ...tle,
        element: {
          ...utopiaComponent,
          rootElement: attachMetadataToElementsInner(utopiaComponent.rootElement),
        },
      }
    } else {
      return tle
    }
  })
}

export function elementDependencyOrdering(
  filename: string,
  code: string,
  elements: Array<TopLevelElementAndCodeContext>,
): Either<ParseFailure, Array<string>> {
  // Build the graph.
  const graph = new Graph()
  function nodeGraphID(element: TopLevelElement): string {
    if (isUtopiaJSXComponent(element)) {
      return element.name
    } else {
      return element.uniqueID
    }
  }
  // Invert the relationships so that we can find the elements that include or reference
  // things by name.
  type DefinedMap = TS.MapLike<Array<TopLevelElement>>
  let definedWithin: DefinedMap = {}
  let definedElsewhere: DefinedMap = {}
  function addToDefinedElsewhere(component: UtopiaJSXComponent, elsewhere: Array<string>): void {
    fastForEach(elsewhere, (e) => {
      definedElsewhere = addToMapOfArraysUnique<TopLevelElement, DefinedMap>(
        definedElsewhere,
        e,
        component,
      )
    })
  }
  function walkJSXAttribute(component: UtopiaJSXComponent, attribute: JSXAttribute): void {
    switch (attribute.type) {
      case 'ATTRIBUTE_OTHER_JAVASCRIPT':
        addToDefinedElsewhere(component, attribute.definedElsewhere)
        break
      case 'ATTRIBUTE_NESTED_ARRAY':
        fastForEach(attribute.content, (attrib) => {
          walkJSXAttribute(component, attrib.value)
        })
        break
      case 'ATTRIBUTE_NESTED_OBJECT':
        fastForEach(attribute.content, (prop) => {
          walkJSXAttribute(component, prop.value)
        })
        break
    }
  }
  function walkJSXAttributes(component: UtopiaJSXComponent, attributes: JSXAttributes): void {
    fastForEach(Object.keys(attributes), (attributeKey) => {
      walkJSXAttribute(component, attributes[attributeKey])
    })
  }
  function walkElement(component: UtopiaJSXComponent, element: JSXElementChild): void {
    if (isJSXElement(element)) {
      // JSXElement
      definedElsewhere = addToMapOfArraysUnique<TopLevelElement, DefinedMap>(
        definedElsewhere,
        element.name.baseVariable,
        component,
      )
      walkJSXAttributes(component, element.props)
      fastForEach(element.children, (child) => {
        walkElement(component, child)
      })
    } else if (isJSXArbitraryBlock(element)) {
      // JSXArbitraryBlock
      addToDefinedElsewhere(component, element.definedElsewhere)
    } else if (isJSXFragment(element)) {
      fastForEach(element.children, (child) => {
        walkElement(component, child)
      })
    }
  }
  for (const elementAndCodeContext of elements) {
    const element = elementAndCodeContext.element
    // Add nodes to the graph.
    const nodeID = nodeGraphID(element)
    if (graph.hasNode(nodeID)) {
      return left(
        parseFailure(null, null, `More than one element with the name ${nodeID}`, [], code),
      )
    } else {
      graph.setNode(nodeID, element)
    }
    // Populate definedWithin and definedElsewhere for the connections.
    if (isUtopiaJSXComponent(element)) {
      definedWithin = addToMapOfArraysUnique<TopLevelElement, DefinedMap>(
        definedWithin,
        element.name,
        element,
      )
      walkElement(element, element.rootElement)
    } else {
      fastForEach(element.definedWithin, (within) => {
        definedWithin = addToMapOfArraysUnique<TopLevelElement, DefinedMap>(
          definedWithin,
          within,
          element,
        )
      })
      fastForEach(element.definedElsewhere, (within) => {
        definedElsewhere = addToMapOfArraysUnique<TopLevelElement, DefinedMap>(
          definedElsewhere,
          within,
          element,
        )
      })
    }
  }
  // Create the connections between things.
  fastForEach(Object.keys(definedWithin), (within) => {
    const elementsWithin = definedWithin[within]
    if (within in definedElsewhere) {
      const elementsElsewhere = definedElsewhere[within]
      fastForEach(elementsElsewhere, (elsewhereElement) => {
        fastForEach(elementsWithin, (withinElement) => {
          graph.setEdge(nodeGraphID(elsewhereElement), nodeGraphID(withinElement))
        })
      })
    }
  })
  const hasCycles = !G.alg.isAcyclic(graph)
  if (hasCycles) {
    const cycles = G.alg.findCycles(graph)
    const firstElementInCycle = forceNotNull(
      'Internal error finding element.',
      elements.find((element) => {
        return nodeGraphID(element.element) === cycles[0][0]
      }),
    )
    const boundsStart = firstElementInCycle.bounds.start
    const boundsEnd = firstElementInCycle.bounds.end
    const startLine = boundsStart.line
    const startColumn = boundsStart.character
    const endLine = boundsEnd.line
    const endColumn = boundsEnd.character
    const errorMessage: ErrorMessage = {
      fileName: filename,
      startLine: startLine,
      startColumn: startColumn,
      endLine: endLine,
      endColumn: endColumn,
      codeSnippet: createCodeSnippetFromCode(code, startLine, startColumn, endLine, endColumn),
      severity: 'fatal',
      type: '',
      message:
        'Circular dependency detected. While this is valid javascript, the Utopia editor cannot currently handle circular dependencies.',
      errorCode: '',
      source: 'utopia-parser',
      passTime: null,
    }
    return left(parseFailure(null, null, null, [errorMessage], code))
  } else {
    const nodeIDs = G.alg.postorder(
      graph,
      elements.map((e) => nodeGraphID(e.element)),
    )
    return right(nodeIDs)
  }
}

export interface CodeWithMap {
  code: string
  sourceMap: RawSourceMap
}

function removeTrailingSemicolon(code: string): string {
  const hasTrailingSemicolon = code[code.length - 1] === ';'
  if (hasTrailingSemicolon) {
    return code.slice(0, -1)
  } else {
    return code
  }
}
export function wrapCodeInParens(code: string): string {
  return `(${removeTrailingSemicolon(code)})`
}

export function wrapCodeInParensWithMap(
  sourceFileName: string,
  sourceFileText: string,
  code: string,
  sourceMap: RawSourceMap,
): CodeWithMap {
  const wrappedCode = wrapCodeInParens(code)

  const consumer = new SourceMapConsumer(sourceMap)
  const node = SourceNode.fromStringWithSourceMap(wrappedCode, consumer)
  node.setSourceContent(sourceFileName, sourceFileText)
  const result = node.toStringWithSourceMap({ file: sourceFileName })
  return { code: result.code, sourceMap: result.map }
}

export function prependToSourceString(
  sourceFileName: string,
  sourceFileText: string,
  sourceCode: string,
  sourceMap: RawSourceMap,
  toPrepend: string,
  toAppend: string,
): CodeWithMap {
  const consumer = new SourceMapConsumer(sourceMap)
  const node = SourceNode.fromStringWithSourceMap(sourceCode + toAppend, consumer)
  node.setSourceContent(sourceFileName, sourceFileText)
  node.prepend(toPrepend)
  const { code, map } = node.toStringWithSourceMap({ file: sourceFileName })
  return {
    code: code,
    sourceMap: JSON.parse(map.toString()),
  }
}

export const JSX_CANVAS_LOOKUP_FUNCTION_NAME = 'utopiaCanvasJSXLookup'

interface ElementWithinInPosition {
  uid: string
  element: JSXElement
  startLine: number
  startColumn: number
}

export type ElementsWithinInPosition = Array<ElementWithinInPosition>
