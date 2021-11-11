import { Either, left, mapEither, right } from '../shared/either'
import { JSXElementChild, UtopiaJSXComponent } from '../shared/element-template'
import { Imports, isParseSuccess } from '../shared/project-file-types'
import { createParseFile, getParseResult, UtopiaTsWorkers } from '../workers/common/worker-types'

export async function getParseResultForUserStrings(
  workers: UtopiaTsWorkers,
  imports: string,
  toInsert: string,
): Promise<Either<string, { parsedImports: Imports; insertionElement: JSXElementChild }>> {
  // TODO instead of calling getParseResult directly,
  // 1. try to use a cached value first, see if the user's input string(s) changed at all
  // 2. we also _probably_ need a queue here, but I'm not sure
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
        const insertionElement = parsedWrapperComponent.rootElement

        return right({
          parsedImports: parsedImports,
          insertionElement: insertionElement,
        })
      }
    }
  }
  return left('There was some error')
}
