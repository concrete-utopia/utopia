import * as Benny from 'benny'
import type { ParseSuccess } from 'utopia-shared/src/types'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { modifyParseSuccessAtPath } from '../../components/editor/store/editor-state'
import { parseProjectContents } from '../../sample-projects/sample-project-utils.test-utils'
import { getAllUniqueUids, getAllUniqueUidsInnerOld, getFilePathForUid } from './get-unique-ids'
import { LargeHydrogenProject } from './test-large-persistent-model.test-utils'
import {
  clearCachedUidsPerFileForTests,
  getUniqeUidsMapping,
  getUniqueUidsMappingInner,
  lookupFilePathForUid,
} from './get-unique-ids-new'

export async function benchmarkGetUniqueUids(): Promise<void> {
  await Benny.suite(
    'COLD RUN collect a mapping of uid -> filename',
    Benny.add('OLD getUniqueUids', async function setup() {
      const parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        const mapping = getAllUniqueUidsInnerOld(parsedProjectContents).filePathToUids
        const result = getFilePathForUid(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids FIFTH', async function setup() {
      const parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        clearCachedUidsPerFileForTests()
        const mapping = getUniqueUidsMappingInner(parsedProjectContents)
        const result = lookupFilePathForUid(
          mapping.filePathToUids,
          '8d4c5e9960f004aebf00a97dabe7cd80',
        )
      }
    }),
    Benny.cycle(),
    Benny.complete(),
  )

  await Benny.suite(
    'ONE FILE CHANGED, memoized collection of uid -> filename',
    Benny.add('OLD getUniqueUids', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        parsedProjectContents = changeOneFileInProjectContents(parsedProjectContents)
        const mapping = getAllUniqueUids(parsedProjectContents).filePathToUids
        const result = getFilePathForUid(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids FIFTH', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        parsedProjectContents = changeOneFileInProjectContents(parsedProjectContents)
        const mapping = getUniqeUidsMapping(parsedProjectContents)
        const result = lookupFilePathForUid(
          mapping.filePathToUids,
          '8d4c5e9960f004aebf00a97dabe7cd80',
        )
      }
    }),
    Benny.cycle(),
    Benny.complete(),
  )

  await Benny.suite(
    'ONE FILE CHANGED, memoized collection of uid -> filename, then one more lookup in the original projectContents',
    Benny.add('OLD getUniqueUids', async function setup() {
      const parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        const changedProjectContents = changeOneFileInProjectContents(parsedProjectContents)
        const newMapping = getAllUniqueUids(changedProjectContents).filePathToUids
        const newResult = getFilePathForUid(newMapping, '8d4c5e9960f004aebf00a97dabe7cd80')

        const oldMapping = getAllUniqueUids(parsedProjectContents).filePathToUids
        const oldResult = getFilePathForUid(oldMapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids FIFTH', async function setup() {
      const parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        const changedProjectContents = changeOneFileInProjectContents(parsedProjectContents)
        const newMapping = getUniqeUidsMapping(parsedProjectContents)
        const newResult = lookupFilePathForUid(
          newMapping.filePathToUids,
          '8d4c5e9960f004aebf00a97dabe7cd80',
        )

        const oldMapping = getUniqeUidsMapping(parsedProjectContents)
        const oldResult = lookupFilePathForUid(
          oldMapping.filePathToUids,
          '8d4c5e9960f004aebf00a97dabe7cd80',
        )
      }
    }),
    Benny.cycle(),
    Benny.complete(),
  )

  await Benny.suite(
    'lookup of only the file based on UID',
    Benny.add('OLD getUniqueUids', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)
      const mapping = getAllUniqueUids(parsedProjectContents).filePathToUids

      return function benchmark() {
        const result = getFilePathForUid(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids FIFTH', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)
      const mapping = getUniqeUidsMapping(parsedProjectContents)

      return function benchmark() {
        const result = lookupFilePathForUid(
          mapping.filePathToUids,
          '8d4c5e9960f004aebf00a97dabe7cd80',
        )
      }
    }),
    Benny.cycle(),
    Benny.complete(),
  )
}

let increment: bigint = BigInt(0)

function changeOneFileInProjectContents(
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  return modifyParseSuccessAtPath(
    '/app/routes/_index/sections/our-promise.jsx',
    { projectContents: projectContents },
    (parseSuccess): ParseSuccess => {
      let newImports = { ...parseSuccess.imports }
      newImports['cica'] = {
        importedAs: null,
        importedWithName: `cica${increment++}`,
        importedFromWithin: [],
      }
      return {
        ...parseSuccess,
        imports: newImports,
      }
    },
    true,
  ).projectContents
}
