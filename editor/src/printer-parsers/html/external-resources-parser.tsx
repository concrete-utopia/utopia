import * as json5 from 'json5'
import * as NodeHTMLParser from 'node-html-parser'
import { useEditorState } from '../../components/editor/store/store-hook'
import { Either, isRight, left, right } from '../../core/shared/either'
import { CodeFile, isCodeFile, ProjectContents } from '../../core/shared/project-file-types'

const googleFontsURIStart = 'https://fonts.googleapis.com/css2?'
const generatedExternalResourcesLinksOpen = '<!-- Begin Generated Utopia External Links -->'
const generatedExternalResourcesLinksClose = '<!-- End Generated Utopia External Links -->'

export function getGeneratedExternalLinkText(htmlFileContents: string): Either<string, string> {
  const startIndex = htmlFileContents.indexOf(generatedExternalResourcesLinksOpen)
  if (startIndex === -1) {
    return left(`Opening comment ${generatedExternalResourcesLinksOpen} not found`)
  }
  const beginningTrimmed = htmlFileContents.slice(
    startIndex + generatedExternalResourcesLinksOpen.length,
  )
  const endIndex = beginningTrimmed.indexOf(generatedExternalResourcesLinksClose)
  if (endIndex === -1) {
    return left(`Closing comment ${generatedExternalResourcesLinksClose} not found`)
  }
  const endTrimmed = beginningTrimmed.slice(0, endIndex).trim()
  return right(endTrimmed.trim())
}

function getPreviewHTMLFileContents(projectContents: ProjectContents): Either<string, CodeFile> {
  const packageJson = projectContents['/package.json']
  if (packageJson != null && isCodeFile(packageJson)) {
    const parsedJSON = json5.parse(packageJson.fileContents)
    if (parsedJSON != null && 'utopia' in parsedJSON) {
      const htmlFilePath = parsedJSON.utopia?.html
      if (htmlFilePath != null) {
        const previewHTMLFileContents = projectContents[`/${htmlFilePath}`]
        if (previewHTMLFileContents != null && isCodeFile(previewHTMLFileContents)) {
          return right(previewHTMLFileContents)
        } else {
          return left(`Path '${htmlFilePath}' could not be found`)
        }
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

function isHTMLElement(node: NodeHTMLParser.Node): node is NodeHTMLParser.HTMLElement {
  return node.nodeType === NodeHTMLParser.NodeType.ELEMENT_NODE
}

interface ExternalResources {
  genericExternalResources: Array<GenericExternalResource>
  googleFontsResources: Array<GoogleFontsResource>
}

interface GenericExternalResource {
  type: 'generic-external-resource'
  href: string
}

function genericExternalResource(href: string): GenericExternalResource {
  return {
    type: 'generic-external-resource',
    href,
  }
}

interface GoogleFontsResource {
  type: 'google-fonts-resource'
  fontFamily: string
  fontStyleParams?: string // placeholder
}

function googleFontsResource(fontFamily: string, fontStyleParams?: string): GoogleFontsResource {
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
        const href = node.getAttribute('href')
        if (href != null) {
          if (href.startsWith(googleFontsURIStart)) {
            const params = href.slice(googleFontsURIStart.length)
            const parsedParams = new URLSearchParams(params)
            const familyParam = parsedParams.get('family')
            if (familyParam != null) {
              googleFontsResources.push(getGoogleFontsResourceFromURL(familyParam))
            } else {
              genericExternalResources.push(genericExternalResource(href))
            }
          } else {
            genericExternalResources.push(genericExternalResource(href))
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

export function useExternalResources(): Either<string, ExternalResources> {
  const { editorState } = useEditorState((store) => ({
    dispatch: store.dispatch,
    editorState: store.editor,
    derivedState: store.derived,
  }))
  const parsedContents = getPreviewHTMLFileContents(editorState.projectContents)
  if (isRight(parsedContents)) {
    const parsedLinkTagsText = getGeneratedExternalLinkText(parsedContents.value.fileContents)
    if (isRight(parsedLinkTagsText)) {
      return parseLinkTags(parsedLinkTagsText.value)
    } else {
      return parsedLinkTagsText
    }
  } else {
    return parsedContents
  }
}
