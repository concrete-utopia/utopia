import * as Benny from 'benny'
import { renderTestEditorWithModel } from '../../components/canvas/ui-jsx.test-utils'
import { getAllUniqueUids } from './get-unique-ids-new'
import { LargeHydrogenProject } from './test-large-persistent-model.test-utils'
import { parseProjectContents } from '../../sample-projects/sample-project-utils.test-utils'

export async function benchmarkGetUniqueUids(): Promise<void> {
  await Benny.suite(
    'collect a mapping of uid -> filename',
    Benny.add('getUniqueUids', async function setup() {
      const parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        const result = getAllUniqueUids(parsedProjectContents)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'benchmarkGetUniqueUids', details: true }),
  )
}
