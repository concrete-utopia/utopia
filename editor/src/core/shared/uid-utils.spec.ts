import { GenerateUID } from './uid-utils'

let someExistingIDs: Set<string> = new Set(['aaa', 'pqr'])

describe('generateConsistentUID', () => {
  it('if the starting value is unused return it', () => {
    const actualResult = GenerateUID.generateConsistentUID(someExistingIDs, 'xyz')
    expect(actualResult).toBe('xyz')
  })
  it('if the starting value is used generate another', () => {
    const actualResult = GenerateUID.generateConsistentUID(someExistingIDs, 'pqr')
    expect(actualResult).toBe('aab')
  })
})
