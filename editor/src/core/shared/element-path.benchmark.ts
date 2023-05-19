import * as Benny from 'benny'
import * as EP from './element-path'

export async function benchmarkElementPathFunction(): Promise<void> {
  await Benny.suite(
    'creating an element path',
    Benny.add('elementPath', () => {
      let workingPathParts: Array<Array<string>> = []
      for (let outerStep = 0; outerStep < 10; outerStep++) {
        let innerArray: Array<string> = []
        for (let innerStep = 0; innerStep < 10; innerStep++) {
          innerArray.push(`${outerStep}-${innerStep}`)
        }
        workingPathParts.push(innerArray)
      }
      return () => {
        EP.elementPath(workingPathParts)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'creating an element path', details: true }),
  )
}
