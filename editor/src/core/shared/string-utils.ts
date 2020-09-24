export function capitalize(value: string): string {
  return value.charAt(0).toUpperCase() + value.slice(1)
}

export function firstLetterIsLowerCase(value: string): boolean {
  return value.length > 0 && isLowerCase(value[0])
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
  return s.replace(/[A-Z]/g, (l) => `-${l.toLowerCase()}`)
}

export function splitIntoLines(s: string): Array<string> {
  return s.split(/[\r\n]/)
}
