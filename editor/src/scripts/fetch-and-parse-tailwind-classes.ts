/* eslint-disable no-console */
import fetch from 'node-fetch'
import fs from 'fs'
import path from 'path'
import * as csstree from 'css-tree'

type AttributeToClassNames = { [attribute: string]: Array<string> }
type ClassNameToAttributes = { [attribute: string]: Array<string> }
interface ParsedClasses {
  attributeToClassNames: AttributeToClassNames
  classNameToAttributes: ClassNameToAttributes
}

function extractClassesFromSelector(node: csstree.CssNode): Array<string> {
  let classes: Array<string> = []
  if (node.type === 'ClassSelector') {
    classes.push(node.name)
  } else if (node.type === 'Selector' || node.type === 'SelectorList') {
    node.children.forEach((c) => classes.push(...extractClassesFromSelector(c)))
  }

  return classes
}

function extractAttributesFromBlockChild(blockChild: csstree.CssNode): Array<string> {
  let attributes: Array<string> = []

  if (blockChild.type === 'Declaration') {
    attributes.push(blockChild.property)
  } else if (blockChild.type === 'DeclarationList') {
    blockChild.children.forEach((c) => attributes.push(...extractAttributesFromBlockChild(c)))
  }

  return attributes
}

function classCombinatorCount(classSelector: string): number {
  return classSelector.split(':').length
}

function removeNegativeOffsetFromFront(classSelector: string): string {
  // We want to group e.g. -inset-1 next to inset-1, as well as combinators containing those
  return classSelector.replace(/^-|(?<=:)-/g, '')
}

function classComparator(a: string, b: string): number {
  // First check if one has no pseudoclasses, as we want those to come first
  const aCount = classCombinatorCount(a)
  const bCount = classCombinatorCount(b)
  const aIsBaseClass = aCount === 1
  const bIsBaseClass = bCount === 1
  if (aIsBaseClass !== bIsBaseClass) {
    return aCount - bCount
  } else {
    // Failing that, remove any negative offset, and sort alphabetically but also supporting the numeric parts
    return removeNegativeOffsetFromFront(a).localeCompare(
      removeNegativeOffsetFromFront(b),
      undefined,
      {
        numeric: true,
      },
    )
  }
}

function parseClasses(rawCSS: string): ParsedClasses {
  let classNameToAttributesSet: { [className: string]: Set<string> } = {}
  let attributeToClassNamesSet: { [attribute: string]: Set<string> } = {}

  const cssAST = csstree.parse(rawCSS)

  csstree.walk(cssAST, (node) => {
    if (node.type === 'Rule' && node.prelude.type === 'SelectorList') {
      let classNames: Array<string> = []
      let attributes: Array<string> = []

      node.prelude.children.forEach((selector) => {
        const unadjustedClassNames = extractClassesFromSelector(selector)
        const withoutSlashes = unadjustedClassNames.map((c) => c.replace(/\\/g, ''))
        classNames.push(...withoutSlashes)
      })

      if (classNames.length > 0) {
        node.block.children.forEach((blockChild) => {
          attributes.push(...extractAttributesFromBlockChild(blockChild))
        })
      }

      const nonVariableAttributes = attributes.filter((a) => !a.startsWith('--'))

      classNames.forEach((className) => {
        let existingCToA = classNameToAttributesSet[className] ?? new Set()
        nonVariableAttributes.forEach((attribute) => existingCToA.add(attribute))
        classNameToAttributesSet[className] = existingCToA
      })

      const baseClassNames = classNames.filter((c) => classCombinatorCount(c) === 1)
      nonVariableAttributes.forEach((attribute) => {
        let existingAToC = attributeToClassNamesSet[attribute] ?? new Set()
        baseClassNames.forEach((className) => existingAToC.add(className))
        attributeToClassNamesSet[attribute] = existingAToC
      })
    }
  })

  let sortedClassNameToAttributes: ClassNameToAttributes = {}
  let sortedAttributeToClassNames: AttributeToClassNames = {}

  Object.keys(classNameToAttributesSet)
    .sort(classComparator)
    .forEach((className) => {
      const attributesSet = classNameToAttributesSet[className] ?? new Set<string>()
      const attributes = Array.from(attributesSet).sort()
      sortedClassNameToAttributes[className] = attributes
    })

  Object.keys(attributeToClassNamesSet)
    .sort()
    .forEach((attribute) => {
      const classNameSet = attributeToClassNamesSet[attribute] ?? new Set<string>()
      const classNames = Array.from(classNameSet).sort(classComparator)
      sortedAttributeToClassNames[attribute] = classNames
    })

  return {
    attributeToClassNames: sortedAttributeToClassNames,
    classNameToAttributes: sortedClassNameToAttributes,
  }
}

async function fetchFullCSS(): Promise<string> {
  const response = await fetch('https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css', {
    method: 'GET',
  })
  if (response.ok) {
    return response.text()
  } else {
    throw new Error(
      `Failed to fetch latest tailwind css (${response.status}): ${response.statusText}`,
    )
  }
}

function createClassNameToAttributesConst(classNameToAttributes: ClassNameToAttributes): string {
  return `export const ClassNameToAttributes: MapLike<Array<string>> = ${JSON.stringify(
    classNameToAttributes,
  )}`
}

function createAllAttributesConst(attributeToClassNames: AttributeToClassNames): string {
  const attributes = Object.keys(attributeToClassNames).sort()
  return `export const AllAttributes: Array<string> = ${JSON.stringify(attributes)}`
}

function createAttributeToClassNamesConst(attributeToClassNames: AttributeToClassNames): string {
  return `export const AttributeToClassNames: MapLike<Array<string>> = ${JSON.stringify(
    attributeToClassNames,
  )}`
}

function createOutputFileContents(parsedClasses: ParsedClasses): string {
  const { attributeToClassNames, classNameToAttributes } = parsedClasses
  return [
    'import type { MapLike } from "typescript"',
    createClassNameToAttributesConst(classNameToAttributes),
    createAllAttributesConst(attributeToClassNames),
    createAttributeToClassNamesConst(attributeToClassNames),
  ].join('\n\n')
}

const SmolSample = [
  String.raw`/*! tailwindcss v2.2.4 | MIT License | https://tailwindcss.com *//*! modern-normalize v1.1.0 | MIT License | https://github.com/sindresorhus/modern-normalize */`,
  String.raw`*,::after,::before{box-sizing:border-box}html{-moz-tab-size:4;tab-size:4}html{line-height:1.15;-webkit-text-size-adjust:100%}body{margin:0}`,
  String.raw`.container{width:100%}@media (min-width:640px){.container{max-width:640px}}@media (min-width:768px){.container{max-width:768px}}@media (min-width:1024px){.container{max-width:1024px}}@media (min-width:1280px){.container{max-width:1280px}}@media (min-width:1536px){.container{max-width:1536px}}`,
  String.raw`.sr-only{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border-width:0}.not-sr-only{position:static;width:auto;height:auto;padding:0;margin:0;overflow:visible;clip:auto;white-space:normal}.focus-within\:sr-only:focus-within{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border-width:0}`,
  String.raw`.focus-within\:not-sr-only:focus-within{position:static;width:auto;height:auto;padding:0;margin:0;overflow:visible;clip:auto;white-space:normal}.focus\:sr-only:focus{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border-width:0}.focus\:not-sr-only:focus{position:static;width:auto;height:auto;padding:0;margin:0;overflow:visible;clip:auto;white-space:normal}`,
  String.raw`.pointer-events-none{pointer-events:none}.pointer-events-auto{pointer-events:auto}.visible{visibility:visible}.invisible{visibility:hidden}.static{position:static}.fixed{position:fixed}.absolute{position:absolute}.relative{position:relative}.sticky{position:sticky}`,
  String.raw`.gap-y-0\.5{row-gap:.125rem}.gap-y-1\.5{row-gap:.375rem}.gap-y-2\.5{row-gap:.625rem}.gap-y-3\.5{row-gap:.875rem}`,
  String.raw`.space-x-0>:not([hidden])~:not([hidden]){--tw-space-x-reverse:0;margin-right:calc(0px * var(--tw-space-x-reverse));margin-left:calc(0px * calc(1 - var(--tw-space-x-reverse)))}.space-x-1>:not([hidden])~:not([hidden]){--tw-space-x-reverse:0;margin-right:calc(.25rem * var(--tw-space-x-reverse));margin-left:calc(.25rem * calc(1 - var(--tw-space-x-reverse)))}.space-x-2>:not([hidden])~:not([hidden]){--tw-space-x-reverse:0;margin-right:calc(.5rem * var(--tw-space-x-reverse));margin-left:calc(.5rem * calc(1 - var(--tw-space-x-reverse)))}`,
  String.raw`.sm\:tabular-nums{--tw-ordinal:var(--tw-empty, );/*!*//*!*/--tw-slashed-zero:var(--tw-empty, );/*!*//*!*/--tw-numeric-figure:var(--tw-empty, );/*!*//*!*/--tw-numeric-spacing:var(--tw-empty, );/*!*//*!*/--tw-numeric-fraction:var(--tw-empty, );/*!*//*!*/font-variant-numeric:var(--tw-ordinal) var(--tw-slashed-zero) var(--tw-numeric-figure) var(--tw-numeric-spacing) var(--tw-numeric-fraction)}.sm\:normal-nums{font-variant-numeric:normal}.sm\:ordinal{--tw-ordinal:ordinal}.sm\:slashed-zero{--tw-slashed-zero:slashed-zero}`,
  String.raw`.sm\:diagonal-fractions,.sm\:lining-nums,.sm\:oldstyle-nums,.sm\:ordinal,.sm\:proportional-nums,.sm\:slashed-zero,.sm\:stacked-fractions,.sm\:tabular-nums{--tw-ordinal:var(--tw-empty, );/*!*//*!*/--tw-slashed-zero:var(--tw-empty, );/*!*//*!*/--tw-numeric-figure:var(--tw-empty, );/*!*//*!*/--tw-numeric-spacing:var(--tw-empty, );/*!*//*!*/--tw-numeric-fraction:var(--tw-empty, );/*!*//*!*/font-variant-numeric:var(--tw-ordinal) var(--tw-slashed-zero) var(--tw-numeric-figure) var(--tw-numeric-spacing) var(--tw-numeric-fraction)}`,
  String.raw`.\32xl\:ease-linear{transition-timing-function:linear}.\32xl\:ease-in{transition-timing-function:cubic-bezier(0.4,0,1,1)}.\32xl\:ease-out{transition-timing-function:cubic-bezier(0,0,0.2,1)}.\32xl\:ease-in-out{transition-timing-function:cubic-bezier(0.4,0,0.2,1)}}`,
].join('')

async function fetchAndParse(useSample: boolean): Promise<void> {
  console.log(`Fetching Tailwind's default CSS from CDN`)
  const rawCSS = useSample ? SmolSample : await fetchFullCSS()
  console.log(`Extracting classes`)
  const parsedClasses = parseClasses(rawCSS)

  console.log(`Writing ${Object.keys(parsedClasses.classNameToAttributes).length} classes to file`)
  const outputFileContents = createOutputFileContents(parsedClasses)
  const targetPath = path.join(__dirname, '..', 'core', 'third-party', 'tailwind-defaults.ts')
  fs.writeFileSync(targetPath, outputFileContents)
  console.log(`Done! Have a nice day :)`)
}

void fetchAndParse(false)
