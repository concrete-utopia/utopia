import * as json5 from 'json5'
import * as NodeHTMLParser from 'node-html-parser'
import * as React from 'react'
import { Either, isRight, left, right } from '../../../core/shared/either'
import { CodeFile, isCodeFile, ProjectContents } from '../../../core/shared/project-file-types'
import { useEditorState } from '../../editor/store/store-hook'

const googleFontsURIStart = 'https://fonts.googleapis.com/css2?'
const generatedExternalResourcesLinksOpen = '<!-- Begin Generated Utopia External Links -->'
const generatedExternalResourcesLinksClose = '<!-- End Generated Utopia External Links -->'

function getGeneratedExternalLinkText(htmlFile: CodeFile): Either<string, string> {
  const startIndex = htmlFile.fileContents.indexOf(generatedExternalResourcesLinksOpen)
  if (startIndex === -1) {
    return left(`Opening comment ${generatedExternalResourcesLinksOpen} not found`)
  }
  const beginningTrimmed = htmlFile.fileContents.slice(
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
  styles: string // placeholder
}

function googleFontsResource(fontFamily: string, styles: string): GoogleFontsResource {
  return {
    type: 'google-fonts-resource',
    fontFamily,
    styles,
  }
}

function getGoogleFontsResourceFromURL(familyParam: string): GoogleFontsResource {
  const dividerIndex = familyParam.indexOf(':')
  const fontFamily = familyParam.slice(0, dividerIndex)
  const fontStyles = familyParam.slice(dividerIndex)
  return googleFontsResource(fontFamily, fontStyles)
}

function parseLinkTags(linkTagsText: string): Either<string, ExternalResources> {
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

function GenericExternalResourcesList(props: { values: Array<GenericExternalResource> }) {
  return null
}
function GoogleFontsResourcesList(props: { values: Array<GoogleFontsResource> }) {
  return null
}

export const ExternalResourcesList = () => {
  const { editorState } = useEditorState((store) => ({
    dispatch: store.dispatch,
    editorState: store.editor,
    derivedState: store.derived,
  }))

  const parsedContents = getPreviewHTMLFileContents(editorState.projectContents)
  let resources: ExternalResources | null = null
  if (isRight(parsedContents)) {
    const parsedLinkTagsText = getGeneratedExternalLinkText(parsedContents.value)
    if (isRight(parsedLinkTagsText)) {
      const parsedLinkTags = parseLinkTags(parsedLinkTagsText.value)
      if (isRight(parsedLinkTags)) {
        resources = parsedLinkTags.value
      }
    }
  }
  if (resources != null) {
    return (
      <>
        {[
          resources.genericExternalResources.length > 0 ? (
            <GenericExternalResourcesList values={resources.genericExternalResources} />
          ) : null,
          resources.googleFontsResources.length > 0 ? (
            <GoogleFontsResourcesList values={resources.googleFontsResources} />
          ) : null,
        ]}
      </>
    )
  } else {
    return null
  }
}
