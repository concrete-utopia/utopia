import { getUtopiaID } from '../../model/element-template-utils'
import { getComponentsFromTopLevelElements } from '../../model/project-file-utils'
import { isJSXElement, JSXElementChild } from '../../shared/element-template'
import { isParseSuccess, ParsedTextFile } from '../../shared/project-file-types'
import { lintAndParse } from './parser-printer'

describe('fixParseSuccessUIDs', () => {
  it('does not fix identical file', () => {
    const newFile = lintAndParse('test.js', baseFileContents, baseFile)
    expect(getUidTree(newFile)).toEqual(getUidTree(baseFile))
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "93b
        c8a
      storyboard-entity
        scene-2-entity
          same-file-app-entity"
    `)
  })

  it('does not die on top level element change', () => {
    const newFile = lintAndParse('test.js', baseFileWithTwoTopLevelComponents, null)
    const newFileFixed = lintAndParse('test.js', baseFileWithTwoTopLevelComponents, baseFile)
    expect(getUidTree(newFileFixed)).toEqual(getUidTree(newFile))
    expect(getUidTree(newFileFixed)).toMatchInlineSnapshot(`
      "7f2
        8de
      93b
        c8a
      storyboard-entity
        scene-2-entity
          same-file-app-entity"
    `)
  })

  it('founds and fixes a single line change', () => {
    const newFile = lintAndParse('test.js', fileWithSingleUpdateContents, baseFile)
    expect(getUidTree(newFile)).toEqual(getUidTree(baseFile))
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "93b
        c8a
      storyboard-entity
        scene-2-entity
          same-file-app-entity"
    `)
  })

  it('avoids uid shifting caused by single prepending insertion', () => {
    const newFile = lintAndParse('test.js', fileWithOneInsertedView, baseFile)
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "93b
        8de
        c8a
      storyboard-entity
        scene-2-entity
          same-file-app-entity"
    `)
  })

  it('double duplication', () => {
    const newFile = lintAndParse('test.js', fileWithTwoDuplicatedViews, baseFile)
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "93b
        c8a
        af7
        a72
      storyboard-entity
        scene-2-entity
          same-file-app-entity"
    `)
  })

  it('insertion at the beginning', () => {
    const threeViews = lintAndParse('test.js', fileWithTwoDuplicatedViews, null)
    const fourViews = lintAndParse('test.js', fileWithTwoDuplicatesAndInsertion, threeViews)
    expect(getUidTree(fourViews)).toMatchInlineSnapshot(`
      "93b
        578
        c8a
        af7
        a72
      storyboard-entity
        scene-2-entity
          same-file-app-entity"
    `)
  })
})

const baseFileContents = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 191,
        }}
      />
    </div>
  )
}
`)

const baseFileWithTwoTopLevelComponents = createFileText(`
export var SecondComponent = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#000000' }}
    >
      <View
        style={{
          width: 100,
        }}
      />
    </div>
  )
}

export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 191,
        }}
      />
    </div>
  )
}
`)

const fileWithSingleUpdateContents = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 101, // <- this is updated
        }}
      />
    </div>
  )
}
`)

const fileWithOneInsertedView = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 100,
        }}
      />
      <View
        style={{
          width: 191,
        }}
      />
    </div>
  )
}
`)

const fileWithTwoDuplicatedViews = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 191,
        }}
      />
      <View
        style={{
          width: 191,
        }}
      />
      <View
        style={{
          width: 191,
        }}
      />
    </div>
  )
}
`)

const fileWithTwoDuplicatesAndInsertion = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 5, // a new View!
        }}
      />
      <View
        style={{
          width: 191,
        }}
      />
      <View
        style={{
          width: 191,
        }}
      />
      <View
        style={{
          width: 191,
        }}
      />
    </div>
  )
}
`)

function createFileText(codeSnippet: string): string {
  return `
  /** @jsx jsx */
  import * as React from 'react'
  import { Scene, Storyboard, jsx } from 'utopia-api'
  import { View } from 'utopia-api'
  
  ${codeSnippet}
  
  export var storyboard = (
    <Storyboard data-uid='storyboard-entity'>
      <Scene
        data-label='Same File App'
        data-uid='scene-2-entity'
        style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
      >
        <SameFileApp data-uid='same-file-app-entity' />
      </Scene>
    </Storyboard>
  )
  `
}

const baseFile = lintAndParse('test.js', baseFileContents, null)

function getUidTree(parsedFile: ParsedTextFile): string {
  if (!isParseSuccess(parsedFile)) {
    return 'FILE NOT PARSE SUCCESS (◕︵◕)'
  } else {
    let printedUidLines: Array<string> = []
    const components = getComponentsFromTopLevelElements(parsedFile.topLevelElements)

    function walkElementChildren(depthSoFar: number, elements: Array<JSXElementChild>): void {
      elements.forEach((element) => {
        const uid = getUtopiaID(element)
        printedUidLines.push(`${'  '.repeat(depthSoFar)}${uid}`)

        if (element != null && isJSXElement(element)) {
          walkElementChildren(depthSoFar + 1, element.children)
        }
      })
    }

    components.forEach((component) => walkElementChildren(0, [component.rootElement]))
    return printedUidLines.join('\n')
  }
}
