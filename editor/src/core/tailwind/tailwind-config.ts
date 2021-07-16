import { RevisionsState, TextFile, textFile, textFileContents } from '../shared/project-file-types'
import { lintAndParse } from '../workers/parser-printer/parser-printer'

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
  textFile(
    textFileContents(
      TailwindConfigJs,
      lintAndParse(TailwindConfigPath, TailwindConfigJs, null, new Set()),
      RevisionsState.BothMatch,
    ),
    null,
    Date.now(),
  )

export const DefaultPostCSSConfig = (): TextFile =>
  textFile(
    textFileContents(
      PostCSSConfigJs,
      lintAndParse(PostCSSPath, PostCSSConfigJs, null, new Set()),
      RevisionsState.BothMatch,
    ),
    null,
    Date.now(),
  )
