import * as json5 from 'json5'
import * as NodeHTMLParser from 'node-html-parser'
import { notice } from '../../components/common/notices'
import { EditorDispatch } from '../../components/editor/action-types'
import { pushToast, updateFile } from '../../components/editor/actions/actions'
import { EditorState, defaultIndexHtmlFilePath } from '../../components/editor/store/editor-state'
import { useEditorState } from '../../components/editor/store/store-hook'
import {
  generatedExternalResourcesLinksClose,
  generatedExternalResourcesLinksOpen,
} from '../../core/model/new-project-files'
import { codeFile } from '../../core/model/project-file-utils'
import { Either, isRight, left, right } from '../../core/shared/either'
import { CodeFile, isCodeFile, ProjectContents } from '../../core/shared/project-file-types'
import { OnSubmitValue } from '../../uuiui-deps'

const googleFontsURIStart = 'https://fonts.googleapis.com/css2?'

function getBoundingStringIndicesForExternalResources(
  htmlFileContents: string,
): Either<string, { startIndex: number; endIndex: number }> {
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
        `Opening comment '${generatedExternalResourcesLinksOpen}' and closing comment '${generatedExternalResourcesLinksClose}' not found`,
      )
    } else if (startIndex === -1) {
      return left(`Opening comment '${generatedExternalResourcesLinksOpen}' not found`)
    } else {
      return left(`Closing comment '${generatedExternalResourcesLinksClose}' not found`)
    }
  }
}

export function getGeneratedExternalLinkText(htmlFileContents: string): Either<string, string> {
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

function getPreviewHTMLFilePath(projectContents: ProjectContents): Either<string, string> {
  const packageJson = projectContents['/package.json']
  if (packageJson != null && isCodeFile(packageJson)) {
    const parsedJSON = json5.parse(packageJson.fileContents)
    if (parsedJSON != null && 'utopia' in parsedJSON) {
      const htmlFilePath = parsedJSON.utopia?.html
      if (htmlFilePath != null) {
        return right(htmlFilePath)
      } else {
        return left(`An html root is not specified in package.json`)
      }
    } else {
      return left(`'utopia' field in package.json couldn't be parsed properly`)
    }
  } else {
    return left('No package.json is found in project')
  }
}

function getCodeFileContentsFromPath(
  filePath: string,
  projectContents: ProjectContents,
): Either<string, CodeFile> {
  const fileContents = projectContents[`/${filePath}`]
  if (fileContents != null && isCodeFile(fileContents)) {
    return right(fileContents)
  } else {
    return left(
      `Path '${projectContents}' could not be found. Check the utopia.html property is set correctly in /package.json`,
    )
  }
}

function isHTMLElement(node: NodeHTMLParser.Node): node is NodeHTMLParser.HTMLElement {
  return node.nodeType === NodeHTMLParser.NodeType.ELEMENT_NODE
}

export interface ExternalResources {
  genericExternalResources: Array<GenericExternalResource>
  googleFontsResources: Array<GoogleFontsResource>
}

export function externalResources(
  genericExternalResources: Array<GenericExternalResource>,
  googleFontsResources: Array<GoogleFontsResource>,
): ExternalResources {
  return { genericExternalResources, googleFontsResources }
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
  fontStyleParams?: string // placeholder
}

export function googleFontsResource(
  fontFamily: string,
  fontStyleParams?: string,
): GoogleFontsResource {
  return {
    type: 'google-fonts-resource',
    fontFamily,
    fontStyleParams,
  }
}

function getGoogleFontsResourceFromURL(familyParam: string): GoogleFontsResource {
  const dividerIndex = familyParam.indexOf(':')
  if (dividerIndex === -1) {
    return googleFontsResource(familyParam)
  } else {
    const fontFamily = familyParam.slice(0, dividerIndex)
    const fontStyleParams = familyParam.slice(dividerIndex)
    return googleFontsResource(fontFamily, fontStyleParams)
  }
}

export function parseLinkTags(linkTagsText: string): Either<string, ExternalResources> {
  const parsed = NodeHTMLParser.parse(linkTagsText)
  if (parsed != null && parsed.valid) {
    let genericExternalResources: Array<GenericExternalResource> = []
    let googleFontsResources: Array<GoogleFontsResource> = []
    parsed.childNodes.forEach((node) => {
      if (isHTMLElement(node) && node.tagName === 'link') {
        const hrefAttribute = node.getAttribute('href')
        const relAttribute = node.getAttribute('rel')
        if (hrefAttribute != null && relAttribute != null) {
          if (hrefAttribute.startsWith(googleFontsURIStart)) {
            const params = hrefAttribute.slice(googleFontsURIStart.length)
            const parsedParams = new URLSearchParams(params)
            const familyParam = parsedParams.get('family')
            if (familyParam != null) {
              googleFontsResources.push(getGoogleFontsResourceFromURL(familyParam))
            } else {
              genericExternalResources.push(genericExternalResource(hrefAttribute, relAttribute))
            }
          } else {
            genericExternalResources.push(genericExternalResource(hrefAttribute, relAttribute))
          }
        }
      }
    })
    return right({
      genericExternalResources,
      googleFontsResources,
    })
  } else {
    return left(`Couldn't parse link tags '${linkTagsText}'`)
  }
}

function printExternalResources(value: ExternalResources): string {
  const generic = value.genericExternalResources.map((resource) => {
    return `<link href="${resource.href}" rel="${resource.rel}">`
  })
  const google = value.googleFontsResources.map((resource) => {
    const encodedFontFamily = encodeURIComponent(resource.fontFamily.replace(' ', '+'))
    return `<link href="${googleFontsURIStart}family=${encodedFontFamily}" rel="stylesheet">`
  })
  return [...generic, ...google].join('\n    ')
}

export function updateHTMLExternalResourcesLinks(
  currentFileContents: string,
  newExternalResources: ExternalResources,
): Either<string, string> {
  const parsedIndices = getBoundingStringIndicesForExternalResources(currentFileContents)
  if (isRight(parsedIndices)) {
    const { startIndex, endIndex } = parsedIndices.value
    const before = currentFileContents.slice(0, startIndex)
    const after = currentFileContents.slice(endIndex)
    return right(
      before +
        generatedExternalResourcesLinksOpen +
        '\n    ' +
        printExternalResources(newExternalResources) +
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
  string,
  { externalResources: ExternalResources; onSubmitValue: OnSubmitValue<ExternalResources> }
> {
  const packageJsonHtmlFilePath = getPreviewHTMLFilePath(editor.projectContents)
  const htmlFilePath: string = isRight(packageJsonHtmlFilePath)
    ? packageJsonHtmlFilePath.value
    : defaultIndexHtmlFilePath

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
          const updatedCodeFileContents = updateHTMLExternalResourcesLinks(fileContents, newValue)
          if (isRight(updatedCodeFileContents)) {
            dispatch([
              updateFile(
                htmlFilePath,
                codeFile(fileContents, updatedCodeFileContents.value),
                false,
              ),
            ])
          } else {
            dispatch([pushToast(notice(updatedCodeFileContents.value))])
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

export function useExternalResources() {
  const { dispatch, editorState } = useEditorState((store) => ({
    editorState: store.editor,
    dispatch: store.dispatch,
  }))
  return getExternalResourcesInfo(editorState, dispatch)
}
