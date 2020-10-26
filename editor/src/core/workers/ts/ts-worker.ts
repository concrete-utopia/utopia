import * as Babel from '@babel/standalone'
import * as ReactSyntaxPlugin from 'babel-plugin-syntax-jsx'
import * as TS from 'typescript'
import * as BrowserFS from 'browserfs'
import { TypeDefinitions } from '../../shared/npm-dependency-types'
import {
  ProjectContents,
  CodeFile,
  UIJSFile,
  isCodeOrUiJsFile,
  ProjectFile,
} from '../../shared/project-file-types'
import { RawSourceMap } from './ts-typings/RawSourceMap'
import { libfile } from './libfile'
import { FSModule } from 'browserfs/dist/node/core/FS'
import { es6dts } from './ts-typings/es6'
import { libdomIterable, libdom } from './ts-typings/dom'
import {
  es2015,
  es2015Symbol,
  es2015SymbolWellknown,
  es2015Reflect,
  es2015Proxy,
  es2015Iterable,
  es2015Generator,
  es2015Collection,
  es2015Core,
  es5,
  es2015Promise,
} from './ts-typings/es2015'
import { libScripthost } from './ts-typings/scripthost'
import { libImportScripts } from './ts-typings/webworker.importscripts'
import { diagnosticToErrorMessage } from './ts-utils'
import { MapLike } from 'typescript'
import { ErrorMessage } from '../../shared/error-messages'
import { fastForEach } from '../../shared/utils'
import { getCodeFileContents } from '../common/project-file-utils'
import infiniteLoopPrevention from '../parser-printer/transform-prevent-infinite-loops'
import { ProjectContentTreeRoot, walkContentsTree } from '../../../components/assets'
import { isDirectory } from '../../model/project-file-utils'
import {
  applyLoaders,
  filenameWithoutJSSuffix,
  loaderExistsForFile,
} from '../../webpack-loaders/loaders'

const TS_LIB_FILES: { [key: string]: string } = {
  'lib.d.ts': libfile,
  'es6.d.ts': es6dts,
  'lib.dom.d.ts': libdom,
  'lib.dom.iterable.d.ts': libdomIterable,
  'lib.es2015.d.ts': es2015,
  'lib.es2015.collection.d.ts': es2015Collection,
  'lib.es2015.core.d.ts': es2015Core,
  'lib.es2015.generator.d.ts': es2015Generator,
  'lib.es2015.iterable.d.ts': es2015Iterable,
  'lib.es2015.promise.d.ts': es2015Promise,
  'lib.es2015.proxy.d.ts': es2015Proxy,
  'lib.es2015.reflect.d.ts': es2015Reflect,
  'lib.es2015.symbol.d.ts': es2015Symbol,
  'lib.es2015.symbol.wellknown.d.ts': es2015SymbolWellknown,
  'lib.es5.d.ts': es5,
  'lib.scripthost.d.ts': libScripthost,
  'lib.webworker.importscripts.d.ts': libImportScripts,
}

let fs: any = null

let fileChanged: (
  filename: string,
  content: string,
  jobID: string,
) => // eslint-disable-next-line @typescript-eslint/no-empty-function
void = () => {}

export type IncomingWorkerMessage = UpdateFileMessage | InitTSWorkerMessage
export type OutgoingWorkerMessage =
  | BuildResultMessage
  | UpdateProcessedMessage
  | InitCompleteMessage

interface UpdateFileMessage {
  type: 'updatefile'
  filename: string
  content: string | ProjectFile
  jobID: string
}

interface InitTSWorkerMessage {
  type: 'inittsworker'
  typeDefinitions: TypeDefinitions
  projectContents: ProjectContentTreeRoot
  buildOrParsePrint: 'build' | 'parse-print'
  jobID: string
}

interface SingleFileBuildResult {
  transpiledCode: string | null
  sourceMap: RawSourceMap | null
  errors: Array<ErrorMessage>
}

export interface MultiFileBuildResult {
  [filename: string]: SingleFileBuildResult
}

export type BuildType = 'full-build' | 'incremental'

export interface BuildResultMessage {
  type: 'build'
  exportsInfo: ReadonlyArray<ExportsInfo>
  buildResult: MultiFileBuildResult
  jobID: string
  buildType: BuildType
}

export interface UpdateProcessedMessage {
  type: 'updateprocessed'
  jobID: string
}

export interface InitCompleteMessage {
  type: 'initcomplete'
  jobID: string
}

export interface FileVersion {
  versionNr: number
  asStringCached: string | null
}

export function filterOldPasses(errorMessages: Array<ErrorMessage>): Array<ErrorMessage> {
  let passTimes: MapLike<number> = {}
  fastForEach(errorMessages, (errorMessage) => {
    if (errorMessage.passTime != null) {
      if (errorMessage.source in passTimes) {
        const existingPassCount = passTimes[errorMessage.source]
        if (errorMessage.passTime > existingPassCount) {
          passTimes[errorMessage.source] = errorMessage.passTime
        }
      } else {
        passTimes[errorMessage.source] = errorMessage.passTime
      }
    }
  })
  return errorMessages.filter((errorMessage) => {
    if (errorMessage.passTime == null) {
      return true
    } else {
      return passTimes[errorMessage.source] === errorMessage.passTime
    }
  })
}

export function createUpdateFileMessage(
  filename: string,
  content: string | UIJSFile | CodeFile,
  jobID: string,
): UpdateFileMessage {
  return {
    type: 'updatefile',
    filename: filename,
    content: content,
    jobID: jobID,
  }
}

export function createInitTSWorkerMessage(
  typeDefinitions: TypeDefinitions,
  projectContents: ProjectContentTreeRoot,
  buildOrParsePrint: 'build' | 'parse-print',
  jobID: string,
): InitTSWorkerMessage {
  return {
    type: 'inittsworker',
    typeDefinitions: typeDefinitions,
    projectContents: projectContents,
    buildOrParsePrint: buildOrParsePrint,
    jobID: jobID,
  }
}

function createBuildResultMessage(
  exportsInfo: Array<ExportsInfo>,
  buildResult: MultiFileBuildResult,
  jobID: string,
  buildType: BuildType,
): BuildResultMessage {
  return {
    type: 'build',
    exportsInfo: exportsInfo,
    buildResult: buildResult,
    jobID: jobID,
    buildType: buildType,
  }
}

function createUpdateProcessedMessage(jobID: string): UpdateProcessedMessage {
  return {
    type: 'updateprocessed',
    jobID: jobID,
  }
}

function createInitCompleteMessage(jobID: string): InitCompleteMessage {
  return {
    type: 'initcomplete',
    jobID: jobID,
  }
}

export interface ExportsInfo {
  filename: string
  code: string
  exportTypes: { [name: string]: ExportType }
}

export type ExportType = {
  type: string
  functionInfo: Array<DetailedTypeInfo> | null
  reactClassInfo: DetailedTypeInfo | null
}

type DetailedTypeInfo = {
  name: string
  memberInfo: { type: string; members: { [member: string]: string } }
}

// FIXME This needs extracting, but getCodeFileContents relies on the parse printer for printing
function getProjectFileContentsAsString(file: ProjectFile): string | null {
  switch (file.type) {
    case 'ASSET_FILE':
      return ''
    case 'DIRECTORY':
      return null
    case 'IMAGE_FILE':
      return file.base64 ?? ''
    case 'CODE_FILE':
    case 'UI_JS_FILE':
      return getCodeFileContents(file, false, true)
    default:
      const _exhaustiveCheck: never = file
      throw new Error(`Unhandled file type ${JSON.stringify(file)}`)
  }
}

export function handleMessage(
  workerMessage: IncomingWorkerMessage,
  sendMessage: (content: OutgoingWorkerMessage) => void,
) {
  // This is a simple implementation to handle changes. If a file content has been updated we do
  // incremental build, but in case of any other change (file moved, deleted, added, etc) we reinitialize
  // the whole project. This is slower, but simple and safe, we wanted to avoid any inconsistency
  // between the main thread and the worker. Most of the changes are just file updates during typing
  // anyway. However, this can be optimized later

  switch (workerMessage.type) {
    case 'inittsworker': {
      initTsIncrementalBuild(
        workerMessage.typeDefinitions,
        workerMessage.projectContents,
        workerMessage.buildOrParsePrint,
        sendMessage,
        workerMessage.jobID,
      )
      break
    }
    case 'updatefile': {
      try {
        let content: string | null
        if (typeof workerMessage.content === 'string') {
          content = workerMessage.content
        } else {
          content = getProjectFileContentsAsString(workerMessage.content)
        }

        if (content != null) {
          fileChanged(workerMessage.filename, content, workerMessage.jobID)
        }
      } finally {
        sendMessage(createUpdateProcessedMessage(workerMessage.jobID))
      }
      break
    }
  }
}

export const DefaultLanguageServiceCompilerOptions: TS.CompilerOptions = {
  noEmitOnError: true,
  noImplicitAny: false,
  target: TS.ScriptTarget.ES2015,
  module: TS.ModuleKind.CommonJS,
  moduleResolution: TS.ModuleResolutionKind.NodeJs,
  jsx: TS.JsxEmit.React,
  allowJs: true,
  allowSyntheticDefaultImports: true,
  esModuleInterop: true,
  outDir: 'build',
  sourceMap: true,
  inlineSources: true,
}

// exported for tests
export function initTsIncrementalBuild(
  typeDefinitions: TypeDefinitions,
  projectContents: ProjectContentTreeRoot,
  buildOrParsePrint: 'build' | 'parse-print',
  sendMessage: (content: OutgoingWorkerMessage) => void,
  jobID: string,
): void {
  initBrowserFS(typeDefinitions, projectContents)
  // Initialize files constituting the program as all .ts files in the current directory
  // Start the watcher
  let codeFiles: Array<string> = []
  let otherFilesToWatch: Array<string> = []
  walkContentsTree(projectContents, (filename, file) => {
    if (isCodeOrUiJsFile(file) && isCssFile(filename)) {
      // FIXME In the bin with this when we introduce a CSS Loader
      otherFilesToWatch.push(filename)
    } else if (!isDirectory(file)) {
      codeFiles.push(filename)
    }
  })
  watch(
    codeFiles,
    otherFilesToWatch,
    typeDefinitions,
    DefaultLanguageServiceCompilerOptions,
    buildOrParsePrint,
    sendMessage,
    jobID,
  )
  sendMessage(createInitCompleteMessage(jobID))
}

export function initBrowserFS(
  typeDefinitions: TypeDefinitions,
  projectContents: ProjectContentTreeRoot,
): FSModule {
  BrowserFS.configure({ fs: 'InMemory', options: {} }, (e) => {
    if (e) {
      throw e
    }
  })

  fs = BrowserFS.BFSRequire('fs')

  // TODO instead of manually having all these dts files in our code ideally a magic webpack plugin could add them.
  Object.keys(TS_LIB_FILES).forEach((lib) => {
    writeFile(fs, `/${lib}`, TS_LIB_FILES[lib])
  })

  walkContentsTree(projectContents, (filename, file) => {
    const fileContents = getProjectFileContentsAsString(file)
    if (fileContents != null) {
      writeFile(fs, filename, fileContents)
    }
  })

  Object.keys(typeDefinitions).forEach((fileName) => {
    const nodeModules = '/node_modules/' + fileName
    writeFile(fs, nodeModules, typeDefinitions[fileName])
  })

  fs.mkdirSync('/build')
  return fs
}

export function writeFileForTests(filename: string, contents: string): void {
  writeFile(fs, filename, contents)
}

function writeFile(fsModule: FSModule, filename: string, contents: string): void {
  const filenameWithStrippedRoot = filename.startsWith('/') ? filename.slice(1) : filename
  const fileNameParts = filenameWithStrippedRoot.split('/')
  const folderParts = fileNameParts.slice(0, -1)
  const longerAndLongerFolderSubPaths = folderParts.map((_, index) => {
    const filePathToCreate = folderParts.slice(0, index + 1)
    return '/' + filePathToCreate.join('/')
  })
  longerAndLongerFolderSubPaths.forEach((folderPart) => {
    if (!fsModule.existsSync(folderPart)) {
      fsModule.mkdirSync(folderPart)
    }
  })
  fsModule.writeFileSync(filename, contents, 'utf8')
}

function getTypeInfoFromClassComponent(
  typeChecker: TS.TypeChecker,
  nodeType: TS.Type,
  type: TS.BaseType,
): {
  name: string
  classInfo: DetailedTypeInfo | null
} {
  let reactClassInfo: DetailedTypeInfo | null = null
  const allAugmentedPropSymbols = typeChecker.getAugmentedPropertiesOfType(nodeType)
  const symbolCalledProps = allAugmentedPropSymbols.find((symbol) => symbol.escapedName === 'props')
  if (symbolCalledProps != null) {
    const reactClassType = typeChecker.getTypeOfSymbolAtLocation(
      symbolCalledProps,
      symbolCalledProps.valueDeclaration,
    )
    const propSymbols = typeChecker.getAugmentedPropertiesOfType(reactClassType)
    let memberInfo: { [name: string]: string } = {}
    if (propSymbols.length > 0) {
      propSymbols.forEach((symbol) => {
        const detailedType =
          // somehow it can have type and no valueDeclaration
          (symbol as any)['type'] != null
            ? (symbol as any)['type']
            : typeChecker.getTypeOfSymbolAtLocation(symbol, symbol.valueDeclaration)
        memberInfo[symbol.name] = typeChecker.typeToString(detailedType)
      })
    }
    reactClassInfo = {
      name: 'props',
      memberInfo: {
        type: typeChecker.typeToString(reactClassType),
        members: memberInfo,
      },
    }
  }
  return {
    name: typeChecker.typeToString(type),
    classInfo: reactClassInfo,
  }
}

function getReactExports(
  typeChecker: TS.TypeChecker,
  node: TS.Node,
  collectedExports: { [name: string]: ExportType },
): { [name: string]: ExportType } {
  if (isNodeExported(node)) {
    const name = (node as any).name && (node as any).name.getText()
    let nodeType: TS.Type
    try {
      // quick fix, sometimes TS throws exception on `getTypeAtLocation`
      nodeType = typeChecker.getTypeAtLocation(node)
    } catch (e) {
      return {
        ...collectedExports,
        [name]: {
          type: 'unknown',
          functionInfo: null,
          reactClassInfo: null,
        },
      }
    }
    const signatures = typeChecker.getSignaturesOfType(nodeType, TS.SignatureKind.Call)
    const isFunction = signatures.length > 0 // it is a function
    let functionInfo: Array<DetailedTypeInfo> = []
    let reactClassInfo: DetailedTypeInfo | null = null
    if (isFunction) {
      const firstSignature = signatures[0]
      if (firstSignature != null) {
        var parameters = firstSignature.getParameters()
        parameters.forEach((param) => {
          const paramType = typeChecker.getTypeOfSymbolAtLocation(param, param.valueDeclaration)
          const augmentedProperties = typeChecker.getAugmentedPropertiesOfType(paramType)
          if (augmentedProperties.length > 0 && (paramType as any)['members'] != null) {
            let memberInfo: { type: string; members: { [member: string]: string } } = {
              type: typeChecker.typeToString(paramType),
              members: {},
            }

            augmentedProperties.forEach((augmentedProp) => {
              const memberType = typeChecker.getTypeOfSymbolAtLocation(
                augmentedProp,
                augmentedProp.valueDeclaration,
              )
              memberInfo.members[augmentedProp.name] = typeChecker.typeToString(memberType)
            })
            functionInfo.push({ name: param.name, memberInfo: memberInfo })
          } else {
            functionInfo.push({
              name: param.name,
              memberInfo: { type: typeChecker.typeToString(paramType), members: {} },
            })
          }
        })
      }
    }

    let typeAsString = typeChecker.typeToString(nodeType)
    const baseTypes = nodeType.getBaseTypes()
    const maybeReactClass =
      TS.isClassLike(node) &&
      nodeType.getApparentProperties().find((p) => p.escapedName === 'render') // 🙈
    if (maybeReactClass && baseTypes != null) {
      typeAsString = baseTypes
        .map((type) => {
          const { name: typeName, classInfo } = getTypeInfoFromClassComponent(
            typeChecker,
            nodeType,
            type,
          )
          reactClassInfo = classInfo
          return typeName
        })
        .join(', ')
    }

    return {
      ...collectedExports,
      [name]: {
        type: typeAsString,
        functionInfo: isFunction ? functionInfo : null,
        reactClassInfo: maybeReactClass ? reactClassInfo : null,
      },
    }
  } else {
    return node
      .getChildren()
      .reduce((working, c) => getReactExports(typeChecker, c, working), collectedExports)
  }
}

function isNodeExported(node: TS.Node): boolean {
  return (
    (TS.isVariableDeclaration(node) ||
      TS.isFunctionDeclaration(node) ||
      TS.isClassDeclaration(node)) &&
    (TS.getCombinedModifierFlags(node) & TS.ModifierFlags.Export) !== 0
  )
}

function existingFilenameToRead(filename: string): string | undefined {
  // Checks that a filename exists that we can load, and returns the filename
  if (loaderExistsForFile(filename) && fs.existsSync(filename)) {
    return filename
  } else {
    const alternativeFilenameToTest = filenameWithoutJSSuffix(filename)
    if (
      alternativeFilenameToTest != null &&
      loaderExistsForFile(alternativeFilenameToTest) &&
      fs.existsSync(alternativeFilenameToTest)
    ) {
      return alternativeFilenameToTest
    } else {
      return undefined
    }
  }
}

export function configureLanguageService(
  rootFilenames: string[],
  fileVersions: TS.MapLike<FileVersion>,
  options: TS.CompilerOptions,
): TS.LanguageService {
  function readFileApplyingLoaders(filename: string): string | undefined {
    const fileVersion = fileVersions[filename]
    if (fileVersion != null && fileVersion.asStringCached != null) {
      return fileVersion.asStringCached
    }

    const filenameToUse = existingFilenameToRead(filename)
    if (filenameToUse == null) {
      return undefined
    }

    const fileContents = fs.readFileSync(filenameToUse, 'utf8').toString()
    const loadedModuleResult = applyLoaders(filenameToUse, fileContents)

    return loadedModuleResult.loadedContents
  }

  // Create the language service host to allow the LS to communicate with the host
  const servicesHost: TS.LanguageServiceHost = {
    getProjectVersion(): string {
      let version: string = ''
      for (const fileKey of Object.keys(fileVersions)) {
        version += `:${fileKey}-${fileVersions[fileKey].versionNr}:`
      }
      return version
    },
    getScriptFileNames: () => {
      return rootFilenames.filter(isJsOrTsFile)
    },
    getScriptVersion: (filename) =>
      fileVersions[filename] && fileVersions[filename].versionNr.toString(),
    getScriptSnapshot: (filename) => {
      const fileContents = readFileApplyingLoaders(filename)
      return fileContents == null ? undefined : TS.ScriptSnapshot.fromString(fileContents)
    },
    getCurrentDirectory: () => '/',
    getCompilationSettings: () => options,
    getDefaultLibFileName: (defaultLibOptions: TS.CompilerOptions) => {
      return 'es6.d.ts'
    },
    fileExists: (filename: string) => {
      return existingFilenameToRead(filename) != null
    },
    readFile: (filename: string) => {
      return readFileApplyingLoaders(filename)
    },
    readDirectory: (
      path: string,
      extensions?: ReadonlyArray<string>,
      exclude?: ReadonlyArray<string>,
      include?: ReadonlyArray<string>,
      depth?: number,
    ) => {
      return fs.readdirSync(path)
    },
  }

  return TS.createLanguageService(servicesHost, TS.createDocumentRegistry())
}

function logErrors(services: TS.LanguageService, fileName: string): Array<ErrorMessage> {
  let allDiagnostics = services
    .getCompilerOptionsDiagnostics()
    .concat(services.getSyntacticDiagnostics(fileName))
    .concat(services.getSemanticDiagnostics(fileName))

  const errorMessages: ErrorMessage[] = allDiagnostics.map(diagnosticToErrorMessage)
  return errorMessages
}

export interface EmitFileResult {
  errors: Array<ErrorMessage>
  transpiledCode: string | null
  sourceMap: RawSourceMap | null
}

export function emitFile(services: TS.LanguageService, filename: string): EmitFileResult {
  if (isTsFile(filename) || isJsFile(filename)) {
    let output = services.getEmitOutput(filename)
    let errorMessages: ErrorMessage[] = []

    if (output.emitSkipped) {
      errorMessages = logErrors(services, filename)
      console.warn(`Emitting ${filename} failed`, errorMessages)
    }
    // TODO: jetpack jetpack we just expect that single file to be built what is changed
    // Although potentially other files (e.g. which import from this) can be affected
    // Let's handle this later, probably not really relevant for js files, where the imports are not checked anyway
    const buildFile = output.outputFiles.find((outputFile) => outputFile.name.endsWith('.js'))
    const sourceMapFile = output.outputFiles.find((outputFile) =>
      outputFile.name.endsWith('.js.map'),
    )

    let transpiledCode = buildFile != null ? buildFile.text : null
    let sourceMap = sourceMapFile != null ? (JSON.parse(sourceMapFile.text) as RawSourceMap) : null

    if (buildFile != null) {
      const babelResult = runBabel(buildFile.text, buildFile.name, sourceMap)
      transpiledCode = babelResult.code
      sourceMap =
        sourceMap != null
          ? {
              ...babelResult.map,
              file: sourceMap.file,
            }
          : null
    }

    return {
      errors: errorMessages,
      transpiledCode: transpiledCode,
      sourceMap: sourceMap,
    }
  } else if (isCssFile(filename)) {
    const content = fs.readFileSync(filename, 'utf8')
    return {
      errors: [],
      transpiledCode: content,
      sourceMap: null,
    }
  } else {
    return {
      errors: [],
      transpiledCode: null,
      sourceMap: null,
    }
  }
}

function watch(
  codeFilesToWatch: string[],
  otherFilesToWatch: string[],
  typeDefinitions: TypeDefinitions,
  options: TS.CompilerOptions,
  buildOrParsePrint: 'build' | 'parse-print',
  sendMessage: (content: OutgoingWorkerMessage) => void,
  jobID: string,
) {
  const fileVersions: TS.MapLike<FileVersion> = {}

  // Initialize the code file version with 0
  ;[...codeFilesToWatch, ...otherFilesToWatch].forEach((filename) => {
    fileVersions[filename] = { versionNr: 0, asStringCached: null }
  })

  Object.keys(TS_LIB_FILES).forEach((lib) => {
    // With and without the forward slash, because reasons.
    fileVersions[lib] = { versionNr: 0, asStringCached: TS_LIB_FILES[lib] }
    fileVersions[`/${lib}`] = { versionNr: 0, asStringCached: TS_LIB_FILES[lib] }
  })

  Object.keys(typeDefinitions).forEach((fileName) => {
    const nodeModulesPath = '/node_modules/' + fileName
    fileVersions[nodeModulesPath] = {
      versionNr: 0,
      asStringCached: typeDefinitions[fileName],
    }
  })

  const services = configureLanguageService(codeFilesToWatch, fileVersions, options)

  // Now let's watch the files
  fileChanged = (filename: string, content: string, jobIDInner: string) => {
    const prevContent = fs.readFileSync(filename, 'utf8')
    const version = fileVersions[filename]
    const contentChanged = prevContent != content

    // if the file has been changed we need to write it to browserfs and increase the version number
    if (contentChanged) {
      // Update the version to signal a change in the file
      fileVersions[filename] = {
        versionNr: version.versionNr + 1,
        asStringCached: content,
      }

      writeFile(fs, filename, content)
    }

    // we only need to emit if the content has been changed or it was not emitted the last time
    if (buildOrParsePrint === 'build') {
      if (contentChanged) {
        // write the output the browserfs
        const buildResult = emitFile(services, filename)
        const parsedExportsInfo = parseExportsInfo(filename)
        const exportsInfo = parsedExportsInfo == null ? [] : [parsedExportsInfo]
        sendMessage(
          createBuildResultMessage(
            exportsInfo,
            {
              [filename]: buildResult,
            },
            jobIDInner,
            'incremental',
          ),
        )
      }
    }
  }

  if (buildOrParsePrint === 'build') {
    let projectBuild: MultiFileBuildResult = {}
    let exportsInfo: Array<ExportsInfo> = []
    ;[...codeFilesToWatch, ...otherFilesToWatch].forEach((rootFile) => {
      const buildResult = emitFile(services, rootFile)
      if (buildResult.transpiledCode != null) {
        projectBuild[rootFile] = buildResult
      }
      const parsedExportsInfo = parseExportsInfo(rootFile)
      if (parsedExportsInfo != null) {
        exportsInfo.push(parsedExportsInfo)
      }
    })
    sendMessage(createBuildResultMessage(exportsInfo, projectBuild, jobID, 'full-build'))
  }

  function parseExportsInfo(fileName: string): ExportsInfo | null {
    if (isTsFile(fileName) || isJsFile(fileName)) {
      let exportTypesInner: { [name: string]: ExportType } = {}
      const fileContent = fs.readFileSync(fileName, 'utf8')
      const program = services.getProgram()
      if (program != null) {
        const sourceFile = program.getSourceFile(fileName)
        if (sourceFile != null) {
          exportTypesInner = getReactExports(program.getTypeChecker(), sourceFile, {})
        }
      }
      return {
        filename: fileName,
        code: fileContent,
        exportTypes: exportTypesInner,
      }
    } else {
      return null
    }
  }
}

function isTsFile(filename: string) {
  return filename.endsWith('.ts') || filename.endsWith('.tsx')
}

function runBabel(code: string, filename: string, sourceMap: RawSourceMap | null) {
  const plugins = [infiniteLoopPrevention]
  return Babel.transform(code, {
    presets: ['es2015'],
    plugins: plugins,
    sourceType: 'script',
    sourceMaps: true,
    inputSourceMap: sourceMap,
    sourceFileName: filename,
  })
}

export function isJsFile(filename: string) {
  return filename.endsWith('.js') || filename.endsWith('.jsx')
}

export function isCssFile(filename: string) {
  return filename.endsWith('.css')
}

export function isJsOrTsFile(filename: string) {
  return isJsFile(filename) || isTsFile(filename)
}

export function isTsLib(filename: string) {
  return TS_LIB_FILES[filename] != null
}
