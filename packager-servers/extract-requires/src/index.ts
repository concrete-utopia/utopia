import { resolveRequirePath } from './extract-require'
import * as path from 'path'
import * as fs from 'fs'

// usage: `npm start "input-file-path.js" "output-file-path.json"`

if (process.argv.length !== 4) {
  throw new Error('invalid number of parameters')
}

const targetPackage = process.argv[2]
const outputParam = process.argv[3]
const packagePath = path.resolve(targetPackage)
const packageName = path.parse(packagePath).name
const result = resolveRequirePath(packagePath, packageName)

const outputPath = path.resolve(outputParam)
fs.writeFileSync(outputPath, JSON.stringify(result, null, 2))
