import { mergeDiff3 } from 'node-diff3'
import {
  currentChangedBranchDeleted,
  currentDeletedBranchChanged,
  differingTypesConflict,
  mergeProjectContentsTree,
} from './github/helpers'

import * as FastCheck from 'fast-check'
import type { ProjectContentsTree } from '../../components/assets'
import { projectContentFile } from '../../components/assets'
import {
  codeFile,
  imageFile,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from './project-file-types'
import { pathArbitrary, projectContentsTreeArbitrary } from '../../components/assets.test-utils'

describe('mergeDiff3', () => {
  it('maintains the spacing between lines that is expected', () => {
    const originCode = `import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { App } from '../src/app'

const root = document.getElementById('root')
if (root != null) {
  ReactDOM.render(<App />, root)
}
`
    const aBranchCode = `import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { App } from '../src/app'

const root = document.getElementById('root')
// A Branch
if (root != null) {
  ReactDOM.render(<App />, root)
}
`
    const bBranchCode = `import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { App } from '../src/app'

const root = document.getElementById('root')
// B Branch
if (root != null) {
  ReactDOM.render(<App />, root)
}
`
    const diffResult = mergeDiff3(
      aBranchCode,
      originCode,
      bBranchCode,

      {
        label: { a: 'A Branch', o: 'Original', b: 'B Branch' },
        stringSeparator: /\r?\n/,
      },
    )
    expect(diffResult.result).toMatchInlineSnapshot(`
      Array [
        "import * as React from 'react'",
        "import * as ReactDOM from 'react-dom'",
        "import { App } from '../src/app'",
        "",
        "const root = document.getElementById('root')",
        "<<<<<<< A Branch",
        "// A Branch",
        "||||||| Original",
        "=======",
        "// B Branch",
        ">>>>>>> B Branch",
        "if (root != null) {",
        "  ReactDOM.render(<App />, root)",
        "}",
        "",
      ]
    `)
  })
})

describe('mergeProjectContentsTree', () => {
  it('should always return a valid result', () => {
    const parameters = FastCheck.tuple(
      pathArbitrary(),
      FastCheck.oneof(projectContentsTreeArbitrary(), FastCheck.constant(null)),
      FastCheck.oneof(projectContentsTreeArbitrary(), FastCheck.constant(null)),
      FastCheck.oneof(projectContentsTreeArbitrary(), FastCheck.constant(null)),
    )
    function checkMergeProjectContentsTree([
      fullPath,
      currentContents,
      originContents,
      branchContents,
    ]: [
      string,
      ProjectContentsTree | null,
      ProjectContentsTree | null,
      ProjectContentsTree | null,
    ]): boolean {
      mergeProjectContentsTree(fullPath, currentContents, originContents, branchContents)
      return true
    }
    const property = FastCheck.property(parameters, checkMergeProjectContentsTree)
    FastCheck.assert(property, { verbose: true })
  })
  it('should return the current contents when a change only exists for that', () => {
    const originContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 0))
    const currentContents = projectContentFile('/myfile.txt', codeFile('current code', null, 1))
    const branchContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 2))
    const mergedContents = projectContentFile(
      '/myfile.txt',
      codeFile('current code', null, 3, RevisionsState.CodeAheadButPleaseTellVSCodeAboutIt),
    )
    const actualResult = mergeProjectContentsTree(
      '/myfile.txt',
      currentContents,
      originContents,
      branchContents,
    )
    expect(actualResult.value).toEqual(mergedContents)
    expect(actualResult.treeConflicts).toEqual({})
  })
  it('should return the branch contents when a change only exists for that', () => {
    const originContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 0))
    const currentContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 1))
    const branchContents = projectContentFile('/myfile.txt', codeFile('branch code', null, 2))
    const mergedContents = projectContentFile('/myfile.txt', codeFile('branch code', null, 3))
    const actualResult = mergeProjectContentsTree(
      '/myfile.txt',
      currentContents,
      originContents,
      branchContents,
    )
    expect(actualResult.value).toEqual(mergedContents)
    expect(actualResult.treeConflicts).toEqual({})
  })
  it('should return the current contents when it is null', () => {
    const originContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 0))
    const currentContents = null
    const branchContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 1))
    const actualResult = mergeProjectContentsTree(
      '/myfile.txt',
      currentContents,
      originContents,
      branchContents,
    )
    expect(actualResult.value).toEqual(currentContents)
    expect(actualResult.treeConflicts).toEqual({})
  })
  it('should return the branch contents when it is null', () => {
    const originContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 0))
    const currentContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 1))
    const branchContents = null
    const actualResult = mergeProjectContentsTree(
      '/myfile.txt',
      currentContents,
      originContents,
      branchContents,
    )
    expect(actualResult.value).toEqual(branchContents)
    expect(actualResult.treeConflicts).toEqual({})
  })
  it('should return merged contents when a change exists in both cases', () => {
    const originContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 0))
    const currentContents = projectContentFile('/myfile.txt', codeFile('current code', null, 1))
    const branchContents = projectContentFile('/myfile.txt', codeFile('branch code', null, 2))
    const mergedCode = `<<<<<<< Your Changes
current code
||||||| Original
origin code
=======
branch code
>>>>>>> Branch Changes`
    const mergedContents = projectContentFile(
      '/myfile.txt',
      codeFile(mergedCode, null, 3, RevisionsState.CodeAheadButPleaseTellVSCodeAboutIt),
    )
    const actualResult = mergeProjectContentsTree(
      '/myfile.txt',
      currentContents,
      originContents,
      branchContents,
    )
    expect(actualResult.value).toEqual(mergedContents)
    expect(actualResult.treeConflicts).toEqual({})
  })
  it('should return a tree conflict if the current contents has changed and the branch contents has changed file type', () => {
    const originContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 0))
    const currentContents = projectContentFile('/myfile.txt', codeFile('current code', null, 1))
    const branchContents = projectContentFile(
      '/myfile.txt',
      imageFile('jpg', undefined, undefined, undefined, 0, undefined),
    )
    const actualResult = mergeProjectContentsTree(
      '/myfile.txt',
      currentContents,
      originContents,
      branchContents,
    )
    expect(actualResult.value).toEqual(currentContents)
    expect(actualResult.treeConflicts).toEqual({
      ['/myfile.txt']: differingTypesConflict(currentContents, originContents, branchContents),
    })
  })
  it('should return a tree conflict if the branch contents has changed and the current contents has changed file type', () => {
    const originContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 0))
    const currentContents = projectContentFile(
      '/myfile.txt',
      imageFile('jpg', undefined, undefined, undefined, 0, undefined),
    )
    const branchContents = projectContentFile('/myfile.txt', codeFile('current code', null, 1))
    const actualResult = mergeProjectContentsTree(
      '/myfile.txt',
      currentContents,
      originContents,
      branchContents,
    )
    expect(actualResult.value).toEqual(currentContents)
    expect(actualResult.treeConflicts).toEqual({
      ['/myfile.txt']: differingTypesConflict(currentContents, originContents, branchContents),
    })
  })
  it('should return a tree conflict if the current contents has changed and the branch contents has been deleted', () => {
    const originContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 0))
    const currentContents = projectContentFile('/myfile.txt', codeFile('current code', null, 1))
    const branchContents = null
    const actualResult = mergeProjectContentsTree(
      '/myfile.txt',
      currentContents,
      originContents,
      branchContents,
    )
    expect(actualResult.value).toEqual(currentContents)
    expect(actualResult.treeConflicts).toEqual({
      ['/myfile.txt']: currentChangedBranchDeleted(currentContents, originContents),
    })
  })
  it('should return a tree conflict if the branch contents has changed and the current contents has been deleted', () => {
    const originContents = projectContentFile('/myfile.txt', codeFile('origin code', null, 0))
    const currentContents = null
    const branchContents = projectContentFile('/myfile.txt', codeFile('current code', null, 1))
    const actualResult = mergeProjectContentsTree(
      '/myfile.txt',
      currentContents,
      originContents,
      branchContents,
    )
    expect(actualResult.value).toEqual(currentContents)
    expect(actualResult.treeConflicts).toEqual({
      ['/myfile.txt']: currentDeletedBranchChanged(originContents, branchContents),
    })
  })
})
