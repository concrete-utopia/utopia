import { getThirdPartyComponents } from './third-party-components'
import { AntdComponents } from './antd-components'

describe('getThirdPartyComponents', () => {
  it('returns the descriptor for antd version 4.3.0', () => {
    const actualResult = getThirdPartyComponents('antd', '4.3.0')
    const expectedResult = AntdComponents['>=4.0.0 <5.0.0']
    expect(actualResult).toBe(expectedResult)
  })
  it('returns nothing for antd version 5.3.0', () => {
    const actualResult = getThirdPartyComponents('antd', '5.3.0')
    expect(actualResult).toBeNull()
  })
  it('returns nothing for antd version 3.3.0', () => {
    const actualResult = getThirdPartyComponents('antd', '3.3.0')
    expect(actualResult).toBeNull()
  })
  it('returns nothing for antantant version 4.3.0', () => {
    const actualResult = getThirdPartyComponents('antantant', '4.3.0')
    expect(actualResult).toBeNull()
  })
})
