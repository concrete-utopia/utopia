// Mocking core/workers/utils.ts because the original contains `import.meta` statements which break in a Node (Jest) environment
jest.mock('./src/core/workers/worker-import-utils')

import { Crypto } from '@peculiar/webcrypto'

globalThis.crypto = new Crypto()
