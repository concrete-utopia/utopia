import * as EP from '../../../../core/shared/element-path'
import { getNavigatorTargetsFromEditorState } from '../../../navigator/navigator-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import type { FragmentLikeType } from './fragment-like-helpers'

export const FragmentLikeElementUid = 'fragment-like'
export const InnerFragmentId = 'inner-fragment'

interface FragmentLikeTagOptions {
  stripTestId: boolean
  stripUids: boolean
  outerUid: string
  innerUid: string
}

const DefaultFragmentLikeTagOptions: FragmentLikeTagOptions = {
  stripTestId: false,
  stripUids: false,
  outerUid: FragmentLikeElementUid,
  innerUid: InnerFragmentId,
}

export function getOpeningFragmentLikeTag(
  type: FragmentLikeType,
  options: Partial<FragmentLikeTagOptions> = DefaultFragmentLikeTagOptions,
): string {
  const optionsFull = { ...DefaultFragmentLikeTagOptions, ...options }
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
      throw new Error(`Unhandled FragmentLikeType ${JSON.stringify(type)}.`)
  }
}

export function getClosingFragmentLikeTag(type: FragmentLikeType): string {
  switch (type) {
    case 'sizeless-div':
      return `</React.Fragment></div>`
    case 'fragment':
      return `</React.Fragment></React.Fragment>`
    case 'conditional':
      return `</React.Fragment> ) : null }`
    default:
      const _exhaustiveCheck: never = type
      throw new Error(`Unhandled FragmentLikeType ${JSON.stringify(type)}.`)
  }
}

export function getRegularNavigatorTargets(renderResult: EditorRenderResult): Array<string> {
  return getNavigatorTargetsFromEditorState(renderResult.getEditorState().editor)
    .navigatorTargets.filter((t) => t.type === 'REGULAR')
    .map((t) => t.elementPath)
    .map(EP.toString)
}
