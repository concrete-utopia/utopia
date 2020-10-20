import * as TS from 'typescript'
import { directoryForFilePath } from '../shared/file-utils'
import {
  createLoaderTransformer,
  ImportMatcher,
  ModuleLoader,
  ModuleLoaderDefaultExport,
} from './ts-loader-factory'

const matcher: ImportMatcher = (importPath: string) => {
  return [/\.avif$/, /\.bmp$/, /\.gif$/, /\.jpe?g$/, /\.png$/].some((regxp) =>
    regxp.test(importPath),
  )
}

const moduleLoader: ModuleLoader = (
  sourceFile: TS.SourceFile,
  importPath: string,
): TS.ObjectLiteralExpression => {
  // { default: url }
  const defaultExport = moduleLoaderDefaultExport(sourceFile, importPath)

  return TS.createObjectLiteral(
    [TS.createPropertyAssignment(TS.createIdentifier('default'), defaultExport)],
    false,
  )
}

const moduleLoaderDefaultExport: ModuleLoaderDefaultExport = (
  sourceFile: TS.SourceFile,
  importPath: string,
): TS.Expression => {
  const directory = directoryForFilePath(sourceFile.fileName)
  const url = `.${directory}/${importPath}` // We have to append a '.' to treat this as a relative path
  return TS.createStringLiteral(url)
}

export const fileLoaderTransform: TS.TransformerFactory<TS.SourceFile> = (
  context: TS.TransformationContext,
): TS.Transformer<TS.SourceFile> => {
  return createLoaderTransformer(context, matcher, moduleLoader, moduleLoaderDefaultExport)
}
