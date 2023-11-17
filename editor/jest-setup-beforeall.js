// Mocking core/workers/utils.ts because the original contains `import.meta` statements which break in a Node (Jest) environment
jest.mock('./src/core/workers/worker-import-utils')

// This supports (currently) dependencies of yjs that require the `crypto` global
// that otherwise is not available in the Jest environment.
import { Crypto } from '@peculiar/webcrypto'
globalThis.crypto = new Crypto()
