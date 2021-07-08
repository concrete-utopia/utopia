/* eslint-disable no-console */
import fetch from 'node-fetch'
import fs from 'fs'
import path from 'path'
import { mapValues } from '../core/shared/object-utils'

const ClassMatcher = new RegExp(String.raw`(?<![\w\-:])\.[^\}]+\}{1}`, 'g')
const CustomPseudoClassMatcher = new RegExp(String.raw`(.+)\\(:.+):\1(.+)`)
const IgnoringCombinatorsMatcher = new RegExp(String.raw`([^>\|\~\+\s]+).*\{(.+)\}`)

function fixCustomPseudoClass(rule: string): string {
  // Tailwind defines custom pseudoclasses by prepending and appending and using '\:' e.g. focus\:sr-only:focus
  const matches = CustomPseudoClassMatcher.exec(rule)
  if (matches == null) {
    return rule
  } else {
    return `${matches[1]}${matches[2]}${matches[3]}`
  }
}

function separateClassNameFromBody(
  rule: string,
): {
  className: string
  body: string
} {
  // Some of the class selectors are defined using combinators, in which case we're only interested in the class name
  const matches = IgnoringCombinatorsMatcher.exec(rule)
  if (matches == null) {
    throw new Error(`Failed to separate rule into className and body for ${rule}`)
  } else {
    const unadjustedClassName = matches[1]
    const className = unadjustedClassName.replace(/\\/g, '')

    return {
      className: className,
      body: matches[2],
    }
  }
}

function extractAttributeKeys(ruleBody: string): Array<string> {
  const lines = ruleBody.split(`;`)
  return lines.map((line) => line.split(':')[0])
}

type ClassNameToAttributes = { [className: string]: Array<string> }

function parseClasses(rawCSS: string): ClassNameToAttributes {
  const matches = rawCSS.match(ClassMatcher)
  let parsedClasses: { [className: string]: Set<string> } = {}

  matches?.forEach((match) => {
    try {
      const withoutDot = match.slice(1) // Remove the dot prefix
      const withFixedPseudoClasses = fixCustomPseudoClass(withoutDot)
      const { className, body } = separateClassNameFromBody(withFixedPseudoClasses)
      const attributes = extractAttributeKeys(body)

      let existing = parsedClasses[className] ?? new Set()
      attributes.forEach((a) => existing.add(a))
      parsedClasses[className] = existing
    } catch (e) {
      throw new Error(`Failed on match ${match}: ${e.message}`)
    }
  })

  return mapValues((set: Set<string>) => Array.from(set), parsedClasses)
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

const SmolSample = [
  String.raw`/*! tailwindcss v2.2.4 | MIT License | https://tailwindcss.com *//*! modern-normalize v1.1.0 | MIT License | https://github.com/sindresorhus/modern-normalize */`,
  String.raw`*,::after,::before{box-sizing:border-box}html{-moz-tab-size:4;tab-size:4}html{line-height:1.15;-webkit-text-size-adjust:100%}body{margin:0}`,
  String.raw`.container{width:100%}@media (min-width:640px){.container{max-width:640px}}@media (min-width:768px){.container{max-width:768px}}@media (min-width:1024px){.container{max-width:1024px}}@media (min-width:1280px){.container{max-width:1280px}}@media (min-width:1536px){.container{max-width:1536px}}.sr-only{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border-width:0}.not-sr-only{position:static;width:auto;height:auto;padding:0;margin:0;overflow:visible;clip:auto;white-space:normal}.focus-within\:sr-only:focus-within{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border-width:0}.focus-within\:not-sr-only:focus-within{position:static;width:auto;height:auto;padding:0;margin:0;overflow:visible;clip:auto;white-space:normal}.focus\:sr-only:focus{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border-width:0}.focus\:not-sr-only:focus{position:static;width:auto;height:auto;padding:0;margin:0;overflow:visible;clip:auto;white-space:normal}.pointer-events-none{pointer-events:none}.pointer-events-auto{pointer-events:auto}.visible{visibility:visible}.invisible{visibility:hidden}.static{position:static}.fixed{position:fixed}.absolute{position:absolute}.relative{position:relative}.sticky{position:sticky}`,
  String.raw`.gap-y-0\.5{row-gap:.125rem}.gap-y-1\.5{row-gap:.375rem}.gap-y-2\.5{row-gap:.625rem}.gap-y-3\.5{row-gap:.875rem}`,
  String.raw`.space-x-0>:not([hidden])~:not([hidden]){--tw-space-x-reverse:0;margin-right:calc(0px * var(--tw-space-x-reverse));margin-left:calc(0px * calc(1 - var(--tw-space-x-reverse)))}.space-x-1>:not([hidden])~:not([hidden]){--tw-space-x-reverse:0;margin-right:calc(.25rem * var(--tw-space-x-reverse));margin-left:calc(.25rem * calc(1 - var(--tw-space-x-reverse)))}.space-x-2>:not([hidden])~:not([hidden]){--tw-space-x-reverse:0;margin-right:calc(.5rem * var(--tw-space-x-reverse));margin-left:calc(.5rem * calc(1 - var(--tw-space-x-reverse)))}`,
  String.raw`.\32xl\:ease-linear{transition-timing-function:linear}.\32xl\:ease-in{transition-timing-function:cubic-bezier(0.4,0,1,1)}.\32xl\:ease-out{transition-timing-function:cubic-bezier(0,0,0.2,1)}.\32xl\:ease-in-out{transition-timing-function:cubic-bezier(0.4,0,0.2,1)}}`,
].join('')

async function fetchAndParse(useSample: boolean): Promise<void> {
  console.log(`Fetching Tailwind's default CSS from CDN`)
  const rawCSS = useSample ? SmolSample : await fetchFullCSS()
  console.log(`Extracting class text`)
  const extractedClasses = parseClasses(rawCSS)
  console.log(`Writing ${Object.keys(extractedClasses).length} classes to file`)
  const targetPath = path.join(__dirname, 'tailwind-class-data.json')
  fs.writeFileSync(targetPath, JSON.stringify(extractedClasses, undefined, 2))
  console.log(`Done! Have a nice day :)`)
}

fetchAndParse(false)
