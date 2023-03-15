import { ContentAffectingType } from './group-like-helpers'

export function getOpeningGroupLikeTag(type: ContentAffectingType): string {
  switch (type) {
    case 'sizeless-div':
      return `<div data-uid='children-affecting' data-testid='children-affecting'><>`
    case 'fragment':
      return `<React.Fragment data-uid='children-affecting' data-testid='children-affecting'><>`
    case 'conditional':
      return `{ true ? ( <>`
    default:
      const _exhaustiveCheck: never = type
      throw new Error(`Unhandled ContentAffectingType ${JSON.stringify(type)}.`)
  }
}

export function getClosingGroupLikeTag(type: ContentAffectingType): string {
  switch (type) {
    case 'sizeless-div':
      return `</></div>`
    case 'fragment':
      return `</></React.Fragment>`
    case 'conditional':
      return `</> ) : null }`
    default:
      const _exhaustiveCheck: never = type
      throw new Error(`Unhandled ContentAffectingType ${JSON.stringify(type)}.`)
  }
}
