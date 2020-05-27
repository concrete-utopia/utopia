const fs = require('fs')

const dtsFile = process.argv[2]
const editorTypingFile = process.argv[3]

try {
  const dts = fs.readFileSync(dtsFile, {encoding: 'utf-8'})
  const newEditorTyping = `export const utopiaApiTypings = \`${dts}\`
`

  fs.writeFileSync(editorTypingFile, newEditorTyping)

  console.log('utopia d.ts updated in editor', editorTypingFile)
} catch(e) {
  console.log(e)
  process.exit(1)
}
