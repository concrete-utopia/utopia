import { GenerateUID } from '../shared/uid-utils'
import Sinon from 'sinon'

function* fakeUids(uids: string[]): Generator<string, any, never> {
  for (const uid of uids) {
    yield uid
  }

  throw new Error('Ran out of mocked uids')
}

export const mockGenerateUid = () => {
  let sandbox: Sinon.SinonSandbox | null = null

  afterEach(() => {
    sandbox?.restore()
    sandbox = null
  })

  return (mockUids: string[]) => {
    sandbox = Sinon.createSandbox()

    const generateUidStub = sandbox.stub(GenerateUID, 'generateUID')
    const generateUidsSource = fakeUids(mockUids)
    generateUidStub.callsFake(() => generateUidsSource.next().value)

    // const generateConsistentUIDStub = sandbox.stub(GenerateUID, 'generateConsistentUID')
    // const generateConsistentUIDStubSource = fakeUids(mockUids)
    // generateConsistentUIDStub.callsFake(() => generateConsistentUIDStubSource.next().value)
  }
}
