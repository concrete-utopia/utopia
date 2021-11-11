import { Either, isRight, left, mapEither, right } from '../shared/either'
import {
  clearJSXElementUniqueIDs,
  JSXElementChild,
  JSXElementWithoutUID,
  UtopiaJSXComponent,
} from '../shared/element-template'
import { jsxSimpleAttributesToProps } from '../shared/jsx-attributes'
import { objectMapDropNulls } from '../shared/object-utils'
import { Imports, isParseSuccess } from '../shared/project-file-types'
import { createParseFile, getParseResult, UtopiaTsWorkers } from '../workers/common/worker-types'

type ProcessedParseResult = Either<
  string,
  { importsToAdd: Imports; elementToInsert: JSXElementWithoutUID }
>
const resultCache: Map<string, ProcessedParseResult> = new Map()

export async function getCachedParseResultForUserStrings(
  workers: UtopiaTsWorkers,
  imports: string,
  toInsert: string,
): Promise<ProcessedParseResult> {
  const cacheKey = imports + toInsert
  const cachedResult = resultCache.get(cacheKey)
  if (cachedResult != null) {
    return cachedResult
  } else {
    const result = await getParseResultForUserStrings(workers, imports, toInsert)
    resultCache.set(cacheKey, result)
    return result
  }
}

async function getParseResultForUserStrings(
  workers: UtopiaTsWorkers,
  imports: string,
  toInsert: string,
): Promise<ProcessedParseResult> {
  const parseResult = await getParseResult(workers, [
    createParseFile(
      'code.tsx',
      `${imports};

       function Utopia$$$Component(props) {
          return (
            ${toInsert}
          )
         }`,
      null,
      Date.now(),
    ),
  ])

  if (parseResult[0].type === 'parsefileresult') {
    const parseFileResult = parseResult[0]
    if (isParseSuccess(parseFileResult.parseResult)) {
      const parsedImports = parseFileResult.parseResult.imports
      const parsedWrapperComponent = parseFileResult.parseResult.topLevelElements.find(
        (topLevelElement): topLevelElement is UtopiaJSXComponent =>
          topLevelElement.type === 'UTOPIA_JSX_COMPONENT' &&
          topLevelElement.name === 'Utopia$$$Component',
      )

      if (parsedWrapperComponent != null) {
        const elementToInsert = clearJSXElementUniqueIDs(parsedWrapperComponent.rootElement)

        if (elementToInsert.type === 'JSX_ELEMENT') {
          return right({
            importsToAdd: parsedImports,
            elementToInsert: elementToInsert,
          })
        }
      }
    }
  }

  // TODO way better error handling
  return left('There was some error')
}
