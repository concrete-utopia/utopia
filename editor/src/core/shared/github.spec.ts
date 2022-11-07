import { mergeDiff3 } from 'node-diff3'

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
