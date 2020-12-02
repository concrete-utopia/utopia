import { MajesticBrokerTestCaseCode } from '../../../test-cases/majestic-broker'
import { getAllUniqueUids } from '../../model/element-template-utils'
import { getComponentsFromTopLevelElements } from '../../model/project-file-utils'
import { uniq } from '../../shared/array-utils'
import { foldParsedTextFile } from '../../shared/project-file-types'
import { testParseCode } from './parser-printer.test-utils'

describe('parseCode', () => {
  it('produces unique IDs for every element', () => {
    const parseResult = testParseCode(MajesticBrokerTestCaseCode)
    foldParsedTextFile(
      (_) => fail('Is a failure.'),
      (success) => {
        const uniqueIDs = getAllUniqueUids(
          getComponentsFromTopLevelElements(success.topLevelElements),
          'Unique IDs failure.',
        )
        expect(uniq(uniqueIDs).length).toMatchInlineSnapshot(`74`)
      },
      (_) => fail('Is unparsed.'),
      parseResult,
    )
  })
})
