import { generateConsistentUID } from './uid-utils'

let someExistingIDs: Set<string> = new Set(['aaa', 'pqr'])

describe('generateConsistentUID', () => {
  it('if the starting value is unused return it', () => {
    const actualResult = generateConsistentUID('xyz', someExistingIDs)
    expect(actualResult).toBe('xyz')
  })
  it('if the starting value is used generate another', () => {
    const actualResult = generateConsistentUID('pqr', someExistingIDs)
    expect(actualResult).toBe('aab')
  })
})
