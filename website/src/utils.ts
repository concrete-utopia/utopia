import { URL_HASH } from './detect-env'

export function addScriptToPage(url: string) {
  const script = document.createElement('script')
  script.src = appendHash(url)
  script.async = true
  document.body.appendChild(script)
}

export function addStyleSheetToPage(url: string) {
  const cssElement = document.createElement('link')
  cssElement.rel = 'stylesheet'
  cssElement.type = 'text/css'
  cssElement.href = appendHash(url)
  document.getElementsByTagName('head')[0].appendChild(cssElement)
}

export function appendHash(url: string): string {
  return `${url}?hash=${URL_HASH}`
}
