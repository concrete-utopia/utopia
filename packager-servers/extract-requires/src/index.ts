import { resolveRequirePath } from './extract-require'
import * as path from 'path'
import * as fs from 'fs'

// usage: `npm start "input-file-path.js" "output-file-path.json"`

if (process.argv.length !== 4) {
  throw new Error('invalid number of parameters')
}

const targetFileParam = process.argv[2]
const outputParam = process.argv[3]
const absoluteUrl = path.resolve(targetFileParam)
const result = resolveRequirePath(absoluteUrl)

const outputPath = path.resolve(outputParam)
fs.writeFileSync(outputPath, JSON.stringify(result, null, 2))
