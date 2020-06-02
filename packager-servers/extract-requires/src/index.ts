import { resolveRequirePath } from './extract-require'
import * as path from 'path'

const absoluteUrl = path.resolve('./test-folder/test-a/index.js')
const result = resolveRequirePath(absoluteUrl)
console.log('result', result)
