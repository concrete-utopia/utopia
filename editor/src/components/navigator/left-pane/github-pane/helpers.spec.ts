import { cleanupBranchName } from './helpers'

describe('cleanupBranchName', () => {
  it('lowercase', async () => {
    expect(cleanupBranchName('something')).toEqual('something')
  })
  it('mixed case', async () => {
    expect(cleanupBranchName('SoMeThinG')).toEqual('SoMeThinG')
  })
  it('with spaces', async () => {
    expect(cleanupBranchName('foo bar baz')).toEqual('foo-bar-baz')
  })
  it('with spaces and extra allowed chars', async () => {
    expect(cleanupBranchName('feat/foo bar baz')).toEqual('feat/foo-bar-baz')
  })
  it('removes trailing .lock', async () => {
    expect(cleanupBranchName('feat/foo bar baz.lock')).toEqual('feat/foo-bar-baz')
  })
  it('with dots', async () => {
    expect(cleanupBranchName('feat.one')).toEqual('feat.one')
  })
  it('removes double dots', async () => {
    expect(cleanupBranchName('feat..one')).toEqual('feat-one')
  })
  it('removes trailing spaces/slashes/dots', async () => {
    expect(cleanupBranchName('feat one            ')).toEqual('feat-one')
    expect(cleanupBranchName('feat one.')).toEqual('feat-one')
    expect(cleanupBranchName('feat one/')).toEqual('feat-one')
  })
  it('removes starting spaces/slashes/dots', async () => {
    expect(cleanupBranchName('            feat one')).toEqual('feat-one')
    expect(cleanupBranchName('.feat one')).toEqual('feat-one')
    expect(cleanupBranchName('/feat one')).toEqual('feat-one')
  })
  it('with unsupported chars', async () => {
    expect(cleanupBranchName('to be, or not_to_be, th@t is THE question: test? yes.')).toEqual(
      'to-be-or-not_to_be-th-t-is-THE-question-test-yes',
    )
  })
  it('removes backslashes', async () => {
    expect(cleanupBranchName('hey\\this')).toEqual('hey-this')
  })
})
