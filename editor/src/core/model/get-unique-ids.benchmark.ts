import * as Benny from 'benny'
import type { ParseSuccess } from 'utopia-shared/src/types'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { modifyParseSuccessAtPath } from '../../components/editor/store/editor-state'
import { parseProjectContents } from '../../sample-projects/sample-project-utils.test-utils'
import { getAllUniqueUids, getAllUniqueUidsInnerOld } from './get-unique-ids'
import {
  clearCachedUidsPerFile,
  getAllUniqueUidsInnerNew,
  getAllUniqueUidsNew,
  lookupFilePathForUid,
} from './get-unique-ids-new'
import {
  clearCachedUidsPerFileThird,
  getAllUniqueUidsInnerNewThird,
  getAllUniqueUidsNewThird,
} from './get-unique-ids-threets'
import { LargeHydrogenProject } from './test-large-persistent-model.test-utils'
import {
  clearCachedUidsPerFileFourth,
  getAllUniqueUidsFourth,
  getAllUniqueUidsInnerFourth,
  lookupFilePathForUidFourth,
} from './get-unique-ids-fourth'
import {
  clearCachedUidsPerFileFifth,
  getAllUniqueUidsFifth,
  getAllUniqueUidsInnerFifth,
  lookupFilePathForUidFifth,
} from './get-unique-ids-fifth'
import {
  clearCachedUidsPerFileSixth,
  getAllUniqueUidsInnerSixth,
  getAllUniqueUidsSixth,
  lookupFilePathForUidSixth,
} from './get-unique-ids-sixth'

export async function benchmarkGetUniqueUids(): Promise<void> {
  await Benny.suite(
    'COLD RUN collect a mapping of uid -> filename',
    Benny.add('OLD getUniqueUids', async function setup() {
      const parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        const mapping = getAllUniqueUidsInnerOld(parsedProjectContents).uidsToFilePaths
        const result = mapping['8d4c5e9960f004aebf00a97dabe7cd80']
      }
    }),
    Benny.add('getUniqueUids NEW', async function setup() {
      const parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        clearCachedUidsPerFile()
        const mapping = getAllUniqueUidsInnerNew(parsedProjectContents)
        const result = lookupFilePathForUid(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids FOURTH', async function setup() {
      const parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        clearCachedUidsPerFileFourth()
        const mapping = getAllUniqueUidsInnerFourth(parsedProjectContents)
        const result = lookupFilePathForUidFourth(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids FIFTH', async function setup() {
      const parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        clearCachedUidsPerFileFifth()
        const mapping = getAllUniqueUidsInnerFifth(parsedProjectContents)
        const result = lookupFilePathForUidFifth(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids SIXTH', async function setup() {
      const parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        clearCachedUidsPerFileSixth()
        const mapping = getAllUniqueUidsInnerSixth(parsedProjectContents)
        const result = lookupFilePathForUidSixth(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids THIRD', async function setup() {
      const parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        clearCachedUidsPerFileThird()
        const mapping = getAllUniqueUidsInnerNewThird(parsedProjectContents)
        const result = mapping.get('8d4c5e9960f004aebf00a97dabe7cd80')
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
        const mapping = getAllUniqueUids(parsedProjectContents).uidsToFilePaths
        const result = mapping['8d4c5e9960f004aebf00a97dabe7cd80']
      }
    }),
    Benny.add('getUniqueUids NEW', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        parsedProjectContents = changeOneFileInProjectContents(parsedProjectContents)
        const mapping = getAllUniqueUidsNew(parsedProjectContents)
        const result = lookupFilePathForUid(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids FOURTH', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        parsedProjectContents = changeOneFileInProjectContents(parsedProjectContents)
        const mapping = getAllUniqueUidsFourth(parsedProjectContents)
        const result = lookupFilePathForUidFourth(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids FIFTH', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        parsedProjectContents = changeOneFileInProjectContents(parsedProjectContents)
        const mapping = getAllUniqueUidsFifth(parsedProjectContents)
        const result = lookupFilePathForUidFifth(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids SIXTH', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        parsedProjectContents = changeOneFileInProjectContents(parsedProjectContents)
        const mapping = getAllUniqueUidsSixth(parsedProjectContents)
        const result = lookupFilePathForUidSixth(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids THIRD', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)

      return function benchmark() {
        parsedProjectContents = changeOneFileInProjectContents(parsedProjectContents)
        const mapping = getAllUniqueUidsNewThird(parsedProjectContents)
        const result = mapping.get('8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.cycle(),
    Benny.complete(),
  )

  await Benny.suite(
    'lookup of only the file based on UID',
    Benny.add('OLD getUniqueUids', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)
      const mapping = getAllUniqueUids(parsedProjectContents).uidsToFilePaths

      return function benchmark() {
        const result = mapping['8d4c5e9960f004aebf00a97dabe7cd80']
      }
    }),
    Benny.add('getUniqueUids NEW', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)
      const mapping = getAllUniqueUidsNew(parsedProjectContents)

      return function benchmark() {
        const result = lookupFilePathForUid(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids FOURTH', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)
      const mapping = getAllUniqueUidsFourth(parsedProjectContents)

      return function benchmark() {
        const result = lookupFilePathForUidFourth(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids FIFTH', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)
      const mapping = getAllUniqueUidsFifth(parsedProjectContents)

      return function benchmark() {
        const result = lookupFilePathForUidFifth(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids SIXTH', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)
      const mapping = getAllUniqueUidsSixth(parsedProjectContents)

      return function benchmark() {
        const result = lookupFilePathForUidSixth(mapping, '8d4c5e9960f004aebf00a97dabe7cd80')
      }
    }),
    Benny.add('getUniqueUids THIRD', async function setup() {
      let parsedProjectContents = parseProjectContents(LargeHydrogenProject.projectContents)
      const mapping = getAllUniqueUidsNewThird(parsedProjectContents)
      const result1 = mapping.get('8d4c5e9960f004aebf00a97dabe7cd80')

      return function benchmark() {
        const result = mapping.get('8d4c5e9960f004aebf00a97dabe7cd80')
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
