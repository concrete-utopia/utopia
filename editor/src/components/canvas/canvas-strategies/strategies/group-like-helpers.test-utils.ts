import * as EP from '../../../../core/shared/element-path'
import { EditorRenderResult } from '../../ui-jsx.test-utils'
import { ContentAffectingType } from './group-like-helpers'

export const GroupLikeElementUid = 'children-affecting'
export const InnerFragmentId = 'inner-fragment'

interface GrouplikeTagOptions {
  stripTestId: boolean
  stripUids: boolean
  outerUid: string
  innerUid: string
}

const DefaultGrouplikeTagOptions: GrouplikeTagOptions = {
  stripTestId: false,
  stripUids: false,
  outerUid: GroupLikeElementUid,
  innerUid: InnerFragmentId,
}

export function getOpeningGroupLikeTag(
  type: ContentAffectingType,
  options: Partial<GrouplikeTagOptions> = DefaultGrouplikeTagOptions,
): string {
  const optionsFull = { ...DefaultGrouplikeTagOptions, ...options }
  const outerDataUid = optionsFull.stripUids === true ? '' : `data-uid='${optionsFull.outerUid}'`
  const outerTestId =
    optionsFull.stripTestId === true ? '' : `data-testid='${optionsFull.outerUid}'`
  const innerDataUid = optionsFull.stripUids === true ? '' : `data-uid='${optionsFull.innerUid}'`

  switch (type) {
    case 'sizeless-div':
      return `<div ${outerDataUid} ${outerTestId}><React.Fragment ${innerDataUid}>`
    case 'fragment':
      return `<React.Fragment ${outerDataUid} ${outerTestId}><React.Fragment ${innerDataUid}>`
    case 'conditional':
      return `{ true /* @utopia/uid=${optionsFull.outerUid} */ ? ( <React.Fragment ${innerDataUid}>`
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

export function getRegularNavigatorTargets(renderResult: EditorRenderResult): Array<string> {
  return renderResult
    .getEditorState()
    .derived.navigatorTargets.filter((t) => t.type === 'REGULAR')
    .map((t) => t.elementPath)
    .map(EP.toString)
}
