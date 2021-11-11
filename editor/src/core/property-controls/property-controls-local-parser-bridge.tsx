import { JSXElementChild } from '../shared/element-template'
import { Imports } from '../shared/project-file-types'
import { createParseFile, getParseResult, UtopiaTsWorkers } from '../workers/common/worker-types'

export async function getParseResultForUserStrings(
  workers: UtopiaTsWorkers,
  imports: string,
  toInsert: string,
): Promise<{ parsedImports: Imports; toInsert: JSXElementChild }> {
  // TODO instead of calling getParseResult directly,
  // 1. try to use a cached value first, see if the user's input string(s) changed at all
  // 2. we also _probably_ need a queue here, but I'm not sure
  getParseResult(workers, [
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
  ]).then((value) => {
    // yay
  })

  return null as any
}
