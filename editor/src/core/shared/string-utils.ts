export function capitalize(value: string): string {
  return value.charAt(0).toUpperCase() + value.slice(1)
}

const aCharCode = 'a'.charCodeAt(0)
const zCharCode = 'z'.charCodeAt(0)

export function firstLetterIsLowerCase(value: string): boolean {
  const charCode = value.charCodeAt(0)
  return !isNaN(charCode) && charCode >= aCharCode && charCode <= zCharCode
}

export function isLowerCase(value: string): boolean {
  return value === value.toLowerCase()
}

export function isEmptyString(value: string | null): boolean {
  return value === null || value === undefined || value.trim().length === 0
}

export function replaceAll(inString: string, searchFor: string, replaceWith: string): string {
  return inString.split(searchFor).join(replaceWith)
}

export function camelCaseToDashed(s: string): string {
  let result: string = ''
  for (let index = 0; index < s.length; index++) {
    const char = s.charAt(index)
    if (char >= 'A' && char <= 'Z') {
      result += '-'
      result += char.toLowerCase()
    } else {
      result += char
    }
  }
  return result
}

export function splitIntoLines(s: string): Array<string> {
  return s.split(/[\r\n]/)
}

export function splitAt(index: number, value: string): [string, string] {
  return [value.slice(0, index), value.slice(index, value.length)]
}

export function trimUpToAndIncluding(prefix: string, target: string): string {
  if (target.includes(prefix)) {
    return target.slice(target.indexOf(prefix) + prefix.length)
  } else {
    return target
  }
}
