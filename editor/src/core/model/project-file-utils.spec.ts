import { addFileToProjectContents } from '../../components/assets'
import { codeFile } from '../shared/project-file-types'
import { getFilePathMappings } from './project-file-utils'

const regularConfigFile = `{
  "compilerOptions": {
    "lib": ["DOM", "DOM.Iterable", "ES2022"],
    "isolatedModules": true,
    "esModuleInterop": true,
    "jsx": "react-jsx",
    "moduleResolution": "Bundler",
    "resolveJsonModule": true,
    "module": "ES2022",
    "target": "ES2022",
    "strict": true,
    "allowJs": true,
    "forceConsistentCasingInFileNames": true,
    "skipLibCheck": true,
    "baseUrl": ".",
    "paths": {
      "@thing/*": ["app/components/thing/*"],
      "~/*": ["app/*"]
    },
    "noEmit": true
  },
  "include": ["./**/*.d.ts", "./**/*.js", "./**/*.jsx"]
}`

const configFileWithComments = `{
  // "compilerOptions": {
  //   "checkJs": false,
  //   "target": "ES2022",
  //   "module": "ES2022",
  //   "moduleResolution": "Bundler",
  //   "baseUrl": ".",
  //   "paths": {
  //     "@thing/*": ["app/components/thing/*"],
  //     "~/*": ["app/*"]
  //   }
  // },
  "compilerOptions": {
    "lib": ["DOM", "DOM.Iterable", "ES2022"],
    "isolatedModules": true,
    "esModuleInterop": true,
    "jsx": "react-jsx",
    "moduleResolution": "Bundler",
    "resolveJsonModule": true,
    "module": "ES2022",
    "target": "ES2022",
    "strict": true,
    "allowJs": true,
    "forceConsistentCasingInFileNames": true,
    "skipLibCheck": true,
    "baseUrl": ".",
    "paths": {
      "@thing/*": ["app/components/thing/*"],
      "~/*": ["app/*"]
    },
    "noEmit": true
  },
  "include": ["./**/*.d.ts", "./**/*.js", "./**/*.jsx"]
}`

const expectedResult: Array<[RegExp, Array<string>]> = [
  [/@thing\/([^/]*)/y, [`/app/components/thing/$1`]],
  [/~\/([^/]*)/y, [`/app/$1`]],
]

describe('getFilePathMappings', () => {
  it('loads from jsconfig.json', () => {
    const projectContents = addFileToProjectContents(
      {},
      'jsconfig.json',
      codeFile(regularConfigFile, null),
    )
    const mappings = getFilePathMappings(projectContents)
    expect(mappings).toEqual(expectedResult)
  })
  it('loads from tsconfig.json', () => {
    const projectContents = addFileToProjectContents(
      {},
      'tsconfig.json',
      codeFile(regularConfigFile, null),
    )
    const mappings = getFilePathMappings(projectContents)
    expect(mappings).toEqual(expectedResult)
  })
  it('handles files with comments in them', () => {
    const projectContents = addFileToProjectContents(
      {},
      'jsconfig.json',
      codeFile(configFileWithComments, null),
    )
    const mappings = getFilePathMappings(projectContents)
    expect(mappings).toEqual(expectedResult)
  })
})
