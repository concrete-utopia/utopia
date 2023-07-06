import type { TextFile } from '../shared/project-file-types'
import { RevisionsState, textFile, textFileContents, unparsed } from '../shared/project-file-types'

export const PostCSSPath = '/postcss.config.js'
export const TailwindConfigPath = '/tailwind.config.js'

const TailwindConfigJs = `
module.exports = {
  purge: [],
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {},
  },
  variants: {},
  plugins: [],
}`

const PostCSSConfigJs = `
module.exports = {
  plugins: {
    tailwindcss: {},
  }
}`

export const DefaultTailwindConfig = (): TextFile =>
  textFile(textFileContents(TailwindConfigJs, unparsed, RevisionsState.CodeAhead), null, null, 0)

export const DefaultPostCSSConfig = (): TextFile =>
  textFile(textFileContents(PostCSSConfigJs, unparsed, RevisionsState.CodeAhead), null, null, 0)
