import type { ComponentElementToInsert } from '../../components/custom-code/code-file'
import type { Either } from '../shared/either'
import { left, right } from '../shared/either'
import type { ArbitraryJSBlock, UtopiaJSXComponent } from '../shared/element-template'
import { getParseCacheOptions } from '../shared/parse-cache-utils'
import type { Imports } from '../shared/project-file-types'
import { isParseFailure, isParseSuccess } from '../shared/project-file-types'
import { emptySet } from '../shared/set-utils'
import { isSteganographyEnabled } from '../shared/stegano-text'
import type { UtopiaTsWorkers } from '../workers/common/worker-types'
import { createParseFile, getParseResult } from '../workers/common/worker-types'

type ProcessedParseResult = Either<
  string,
  { importsToAdd: Imports; elementToInsert: ComponentElementToInsert }
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
  const codeToParse = `${imports};

  function Utopia$$$Component(props) {
     return (
       ${toInsert}
     )
    }`
  const parseResult = await getParseResult(
    workers,
    [createParseFile('code.tsx', codeToParse, null, Date.now())],
    [],
    emptySet(),
    isSteganographyEnabled(),
    getParseCacheOptions(),
  )

  if (parseResult[0].type === 'parsefileresult') {
    const parseFileResult = parseResult[0]
    if (isParseSuccess(parseFileResult.parseResult)) {
      const parsedImports = parseFileResult.parseResult.imports
      const parsedWrapperComponent = parseFileResult.parseResult.topLevelElements.find(
        (topLevelElement): topLevelElement is UtopiaJSXComponent =>
          topLevelElement.type === 'UTOPIA_JSX_COMPONENT' &&
          topLevelElement.name === 'Utopia$$$Component',
      )
      const parsedWrapperIsArbitrary = parseFileResult.parseResult.topLevelElements.find(
        (topLevelElement): topLevelElement is ArbitraryJSBlock =>
          topLevelElement.type === 'ARBITRARY_JS_BLOCK' &&
          topLevelElement.definedWithin.includes('Utopia$$$Component'),
      )
      if (parsedWrapperComponent != null) {
        const elementToInsert = parsedWrapperComponent.rootElement

        if (
          elementToInsert.type === 'JSX_ELEMENT' ||
          elementToInsert.type === 'JSX_FRAGMENT' ||
          elementToInsert.type === 'JSX_MAP_EXPRESSION' ||
          elementToInsert.type === 'JSX_CONDITIONAL_EXPRESSION'
        ) {
          return right({
            importsToAdd: parsedImports,
            elementToInsert: elementToInsert,
          })
        } else {
          return left(
            'Element to insert must be a correct jsx element, fragment, conditional expression or map expression',
          )
        }
      } else if (parsedWrapperIsArbitrary != null) {
        const missingElements = parsedWrapperIsArbitrary.definedElsewhere.filter(
          (v) => v !== 'React' && v !== 'utopiaCanvasJSXLookup',
        )
        return left(
          `Element cannot be inserted without its import statement. Make sure to import ${missingElements.join(
            ', ',
          )}`,
        )
      } else {
        return left('could not find Utopia$$$Component')
      }
    } else if (isParseFailure(parseFileResult.parseResult)) {
      // TODO better error messages!
      return left(parseFileResult.parseResult.errorMessages.map((em) => em.message).join(', '))
    }
  }

  return left('Unknown error')
}
