import * as json5 from 'json5'
import * as NodeHTMLParser from 'node-html-parser'
import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../../components/assets'
import { notice } from '../../components/common/notices'
import { EditorDispatch } from '../../components/editor/action-types'
import { pushToast, updateFile } from '../../components/editor/actions/actions'
import { defaultIndexHtmlFilePath, EditorState } from '../../components/editor/store/editor-state'
import { useEditorState } from '../../components/editor/store/store-hook'
import {
  useCallbackFactory,
  UseSubmitValueFactory,
} from '../../components/inspector/common/property-path-hooks'
import {
  WebFontVariant,
  webFontVariant,
  WebFontWeight,
  isFontVariantWeight,
} from '../../components/navigator/external-resources/google-fonts-utils'
import {
  generatedExternalResourcesLinksClose,
  generatedExternalResourcesLinksOpen,
} from '../../core/model/new-project-files'
import { codeFile } from '../../core/model/project-file-utils'
import { Either, isRight, left, mapEither, right } from '../../core/shared/either'
import { CodeFile, isCodeFile, ProjectContents } from '../../core/shared/project-file-types'
import { NO_OP } from '../../core/shared/utils'
import { DescriptionParseError, descriptionParseError } from '../../utils/value-parser-utils'
import { OnSubmitValue } from '../../uuiui-deps'

const googleFontsURIBase = 'https://fonts.googleapis.com/css2'

function getBoundingStringIndicesForExternalResources(
  htmlFileContents: string,
): Either<DescriptionParseError, { startIndex: number; endIndex: number }> {
  const startIndex = htmlFileContents.indexOf(generatedExternalResourcesLinksOpen)
  const endIndex =
    htmlFileContents.indexOf(generatedExternalResourcesLinksClose) +
    generatedExternalResourcesLinksClose.length
  if (startIndex > -1 && endIndex > -1) {
    return right({
      startIndex,
      endIndex,
    })
  } else {
    if (startIndex === -1 && endIndex === -1) {
      return left(
        descriptionParseError(
          `Opening comment '${generatedExternalResourcesLinksOpen}' and closing comment '${generatedExternalResourcesLinksClose}' not found`,
        ),
      )
    } else if (startIndex === -1) {
      return left(
        descriptionParseError(`Opening comment '${generatedExternalResourcesLinksOpen}' not found`),
      )
    } else {
      return left(
        descriptionParseError(
          `Closing comment '${generatedExternalResourcesLinksClose}' not found`,
        ),
      )
    }
  }
}

/** Does not include the opening and closing comments */
export function getGeneratedExternalLinkText(
  htmlFileContents: string,
): Either<DescriptionParseError, string> {
  const parsedIndices = getBoundingStringIndicesForExternalResources(htmlFileContents)
  if (isRight(parsedIndices)) {
    const { startIndex, endIndex } = parsedIndices.value
    const beginningTrimmed = htmlFileContents.slice(
      startIndex + generatedExternalResourcesLinksOpen.length,
    )
    const endTrimmed = beginningTrimmed
      .slice(
        0,
        endIndex -
          startIndex -
          generatedExternalResourcesLinksOpen.length -
          generatedExternalResourcesLinksClose.length,
      )
      .trim()
    return right(endTrimmed.trim())
  } else {
    return parsedIndices
  }
}

function getPreviewHTMLFilePath(
  projectContents: ProjectContentTreeRoot,
): Either<DescriptionParseError, string> {
  const packageJson = getContentsTreeFileFromString(projectContents, '/package.json')
  if (packageJson != null && isCodeFile(packageJson)) {
    try {
      const parsedJSON = json5.parse(packageJson.fileContents)
      if (parsedJSON != null && 'utopia' in parsedJSON) {
        const htmlFilePath = parsedJSON.utopia?.html
        if (htmlFilePath != null) {
          return right(htmlFilePath)
        } else {
          return left(descriptionParseError(`An html root is not specified in package.json`))
        }
      } else {
        return left(
          descriptionParseError(`'utopia' field in package.json couldn't be parsed properly`),
        )
      }
    } catch (e) {
      return left(descriptionParseError(`package.json is not formatted correctly`))
    }
  } else {
    return left(descriptionParseError('No package.json is found in project'))
  }
}

function getCodeFileContentsFromPath(
  filePath: string,
  projectContents: ProjectContentTreeRoot,
): Either<DescriptionParseError, CodeFile> {
  const fileContents = getContentsTreeFileFromString(projectContents, filePath)
  if (fileContents != null && isCodeFile(fileContents)) {
    return right(fileContents)
  } else {
    return left(
      descriptionParseError(
        `Path '${projectContents}' could not be found. Check the utopia.html property is set correctly in /package.json`,
      ),
    )
  }
}

function isHTMLElement(node: NodeHTMLParser.Node): node is NodeHTMLParser.HTMLElement {
  return node.nodeType === NodeHTMLParser.NodeType.ELEMENT_NODE
}

export interface ExternalResources {
  type: 'external-resources'
  genericExternalResources: Array<GenericExternalResource>
  googleFontsResources: Array<GoogleFontsResource>
}

export function externalResources(
  genericExternalResources: Array<GenericExternalResource>,
  googleFontsResources: Array<GoogleFontsResource>,
): ExternalResources {
  return {
    type: 'external-resources',
    genericExternalResources,
    googleFontsResources,
  }
}

// TODO: support arbitrary attributes
export interface GenericExternalResource {
  type: 'generic-external-resource'
  href: string
  rel: string
}

export function genericExternalResource(href: string, rel: string): GenericExternalResource {
  return {
    type: 'generic-external-resource',
    href,
    rel,
  }
}

export interface GoogleFontsResource {
  type: 'google-fonts-resource'
  fontFamily: string
  variants: Array<WebFontVariant>
  otherQueryStringParams?: string
}

export function googleFontsResource(
  fontFamily: string,
  variants: Array<WebFontVariant>,
  otherQueryStringParams?: string,
): GoogleFontsResource {
  return {
    type: 'google-fonts-resource',
    fontFamily,
    variants,
    otherQueryStringParams,
  }
}

function axisTuplesToFontVariant(axisTuples: AxisTuples): Array<WebFontVariant> {
  return axisTuples.map((axisTuple) =>
    webFontVariant(axisTuple[1], axisTuple[0] === 1 ? 'italic' : 'normal'),
  )
}

type ItalicAxisValue = 0 | 1
function isItalicAxisValue(value: number): value is ItalicAxisValue {
  return value === 0 || value === 1
}
type AxisTuple = [ItalicAxisValue, WebFontWeight]
type AxisTuples = Array<AxisTuple>

function recursivelyParseAxisTuples(
  remaining: string,
  workingAxisValue: string = '',
  workingAxisTuple: [] | [ItalicAxisValue] = [],
  workingAxisTuples: AxisTuples = [],
): Either<DescriptionParseError, AxisTuples> {
  const currentCharacter = remaining[0]
  if (currentCharacter == null) {
    const italValue = workingAxisTuple[0]
    if (italValue != null) {
      const wghtValue = Number(workingAxisValue)
      if (isFontVariantWeight(wghtValue)) {
        const lastTuple: AxisTuple = [italValue, wghtValue]
        const finalTuples = [...workingAxisTuples, lastTuple]
        return right(finalTuples)
      } else {
        return left(descriptionParseError(`${wghtValue} is not a valid font-weight keyword value`))
      }
    } else {
      return left(descriptionParseError('Font axis tuple list is not properly formed'))
    }
  }

  const nextRemaining = remaining.slice(1)

  switch (currentCharacter) {
    case ',': {
      const italValue = Number(workingAxisValue)
      if (isItalicAxisValue(italValue)) {
        return recursivelyParseAxisTuples(nextRemaining, '', [italValue], workingAxisTuples)
      } else {
        return left(descriptionParseError(`Tuple value ${italValue} is not a number`))
      }
    }
    case ';': {
      const italValue = workingAxisTuple[0]
      if (italValue != null) {
        const wghtValue = Number(workingAxisValue)
        if (isFontVariantWeight(wghtValue)) {
          const workingNextTuple: AxisTuple = [italValue, wghtValue]
          let workingNextTuples = [...workingAxisTuples]
          workingNextTuples.push(workingNextTuple)
          return recursivelyParseAxisTuples(nextRemaining, '', [], workingNextTuples)
        } else {
          return left(descriptionParseError(`Tuple value ${wghtValue} is not a number`))
        }
      } else {
        return left(descriptionParseError('Tuple ended too early'))
      }
    }
    default: {
      return recursivelyParseAxisTuples(
        nextRemaining,
        workingAxisValue + currentCharacter,
        workingAxisTuple,
        workingAxisTuples,
      )
    }
  }
}

function parseVariantsFromAxisLists(
  params: string,
): Either<DescriptionParseError, Array<WebFontVariant>> {
  if (params.startsWith('ital,wght@')) {
    const tuplesString = params.slice('ital,wght@'.length)
    const parsedTuples = recursivelyParseAxisTuples(tuplesString)
    return mapEither(axisTuplesToFontVariant, parsedTuples)
  } else {
    return left(
      descriptionParseError('Font variant definition is not properly formed for the parser.'),
    )
  }
}

function getGoogleFontsResourceFromURL(
  familyParam: string,
  otherQueryStringParams: string,
): Either<DescriptionParseError, GoogleFontsResource> {
  const dividerIndex = familyParam.indexOf(':')
  if (dividerIndex === -1) {
    return right(
      googleFontsResource(familyParam, [webFontVariant(400, 'normal')], otherQueryStringParams),
    )
  } else {
    const fontFamily = familyParam.slice(0, dividerIndex)
    const axisLists = familyParam.slice(dividerIndex + 1)
    const parsedVariants = parseVariantsFromAxisLists(axisLists)
    return mapEither(
      (r) => googleFontsResource(fontFamily, r, otherQueryStringParams),
      parsedVariants,
    )
  }
}

export function parseLinkTags(
  linkTagsText: string,
): Either<DescriptionParseError, ExternalResources> {
  const parsed = NodeHTMLParser.parse(linkTagsText)
  if (parsed != null && parsed.valid) {
    let genericExternalResources: Array<GenericExternalResource> = []
    let googleFontsResources: Array<GoogleFontsResource> = []
    parsed.childNodes.forEach((node) => {
      if (isHTMLElement(node) && node.tagName === 'link') {
        const hrefAttribute = node.getAttribute('href')
        const relAttribute = node.getAttribute('rel')
        if (hrefAttribute != null && relAttribute != null) {
          if (hrefAttribute.startsWith(googleFontsURIBase)) {
            let parsedParams = new URL(hrefAttribute).searchParams
            const familyParam = parsedParams.get('family')
            parsedParams.delete('family')
            const otherParams = parsedParams.toString()
            if (familyParam != null) {
              const parsedResource = getGoogleFontsResourceFromURL(familyParam, otherParams)
              if (isRight(parsedResource)) {
                googleFontsResources.push(parsedResource.value)
              } else {
                genericExternalResources.push(genericExternalResource(hrefAttribute, relAttribute))
              }
            } else {
              genericExternalResources.push(genericExternalResource(hrefAttribute, relAttribute))
            }
          } else {
            genericExternalResources.push(genericExternalResource(hrefAttribute, relAttribute))
          }
        }
      }
    })
    return right(
      externalResources(
        genericExternalResources,
        googleFontsResources.sort((a, b) => a.fontFamily.localeCompare(b.fontFamily, 'en')),
      ),
    )
  } else {
    return left(descriptionParseError(`Couldn't parse link tags '${linkTagsText}'`))
  }
}

function printVariantAxisTuples(variants: Array<WebFontVariant>): string {
  return variants.length > 0
    ? `:ital,wght@${variants
        .map((variant) => {
          return `${variant.webFontStyle === 'italic' ? 1 : 0},${variant.webFontWeight}`
        })
        .join(';')}`
    : ''
}

function replaceSafeGoogleFontsCharacters(value: string): string {
  return value.replace(/%3A/g, ':').replace(/%3B/g, ';').replace(/%2C/g, ',').replace(/%40/g, '@')
}

export function printExternalResources(value: ExternalResources): string {
  const generic = value.genericExternalResources.map((resource) => {
    return `<link href="${resource.href}" rel="${resource.rel}">`
  })
  const google = value.googleFontsResources.map((resource) => {
    const searchParams = new URLSearchParams(resource.otherQueryStringParams)
    const variantAxisTuples = printVariantAxisTuples(resource.variants)
    searchParams.append('family', resource.fontFamily + variantAxisTuples)
    const prettySearchParams = replaceSafeGoogleFontsCharacters(searchParams.toString())
    const url = new URL(`${googleFontsURIBase}?${prettySearchParams}`)
    return `<link href="${url.toString()}" rel="stylesheet">`
  })
  return [...generic, ...google].join('\n    ')
}

export function updateHTMLExternalResourcesLinks(
  currentFileContents: string,
  newExternalResources: string,
): Either<DescriptionParseError, string> {
  const parsedIndices = getBoundingStringIndicesForExternalResources(currentFileContents)
  if (isRight(parsedIndices)) {
    const { startIndex, endIndex } = parsedIndices.value
    const before = currentFileContents.slice(0, startIndex)
    const after = currentFileContents.slice(endIndex)
    return right(
      before +
        generatedExternalResourcesLinksOpen +
        '\n    ' +
        newExternalResources +
        '\n    ' +
        generatedExternalResourcesLinksClose +
        after,
    )
  } else {
    return parsedIndices
  }
}

export function getExternalResourcesInfo(
  editor: EditorState,
  dispatch: EditorDispatch,
): Either<
  DescriptionParseError,
  { externalResources: ExternalResources; onSubmitValue: OnSubmitValue<ExternalResources> }
> {
  const packageJsonHtmlFilePath = getPreviewHTMLFilePath(editor.projectContents)
  const htmlFilePath: string = `/${
    isRight(packageJsonHtmlFilePath) ? packageJsonHtmlFilePath.value : defaultIndexHtmlFilePath
  }`

  const previewHTMLFilePathContents = getCodeFileContentsFromPath(
    htmlFilePath,
    editor.projectContents,
  )
  if (isRight(previewHTMLFilePathContents)) {
    const fileContents = previewHTMLFilePathContents.value.fileContents
    const parsedLinkTagsText = getGeneratedExternalLinkText(fileContents)
    if (isRight(parsedLinkTagsText)) {
      const parsedExternalResources = parseLinkTags(parsedLinkTagsText.value)
      if (isRight(parsedExternalResources)) {
        function onSubmitValue(newValue: ExternalResources) {
          const updatedCodeFileContents = updateHTMLExternalResourcesLinks(
            fileContents,
            printExternalResources(newValue),
          )
          if (isRight(updatedCodeFileContents)) {
            dispatch([
              updateFile(
                htmlFilePath,
                codeFile(updatedCodeFileContents.value, updatedCodeFileContents.value),
                false,
              ),
            ])
          } else {
            dispatch([pushToast(notice(updatedCodeFileContents.value.description))])
          }
        }
        return right({ externalResources: parsedExternalResources.value, onSubmitValue })
      } else {
        return parsedExternalResources
      }
    } else {
      return parsedLinkTagsText
    }
  } else {
    return previewHTMLFilePathContents
  }
}

export function useExternalResources(): {
  values: Either<DescriptionParseError, ExternalResources>
  onSubmitValue: OnSubmitValue<ExternalResources>
  useSubmitValueFactory: UseSubmitValueFactory<ExternalResources>
} {
  const { dispatch, editorState } = useEditorState(
    (store) => ({
      editorState: store.editor,
      dispatch: store.dispatch,
    }),
    'useExternalResources',
  )
  const externalResourcesInfo = getExternalResourcesInfo(editorState, dispatch)
  const values: Either<DescriptionParseError, ExternalResources> = isRight(externalResourcesInfo)
    ? right(externalResourcesInfo.value.externalResources)
    : left(externalResourcesInfo.value)
  const onSubmitValue = isRight(externalResourcesInfo)
    ? externalResourcesInfo.value.onSubmitValue
    : NO_OP
  const useSubmitValueFactory = useCallbackFactory(
    isRight(values) ? values.value : externalResources([], []),
    onSubmitValue,
  )
  return {
    values,
    onSubmitValue,
    useSubmitValueFactory,
  }
}
