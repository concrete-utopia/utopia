// Mocking core/workers/utils.ts because the original contains `import.meta` statements which break in a Node (Jest) environment
jest.mock('./src/core/workers/utils')
