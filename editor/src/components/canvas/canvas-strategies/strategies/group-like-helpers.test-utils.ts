import { ContentAffectingType } from './group-like-helpers'

export function getOpeningGroupLikeTag(type: ContentAffectingType): string {
  switch (type) {
    case 'sizeless-div':
      return `<div data-uid='children-affecting' data-testid='children-affecting'><React.Fragment data-uid='inner-fragment'>`
    case 'fragment':
      return `<React.Fragment data-uid='children-affecting' data-testid='children-affecting'><React.Fragment data-uid='inner-fragment'>`
    case 'conditional':
      return `{ true /* @utopia/uid=children-affecting */ ? ( <React.Fragment data-uid='inner-fragment'>`
    default:
      const _exhaustiveCheck: never = type
      throw new Error(`Unhandled ContentAffectingType ${JSON.stringify(type)}.`)
  }
}

export function getClosingGroupLikeTag(type: ContentAffectingType): string {
  switch (type) {
    case 'sizeless-div':
      return `</React.Fragment></div>`
    case 'fragment':
      return `</React.Fragment></React.Fragment>`
    case 'conditional':
      return `</React.Fragment> ) : null }`
    default:
      const _exhaustiveCheck: never = type
      throw new Error(`Unhandled ContentAffectingType ${JSON.stringify(type)}.`)
  }
}
