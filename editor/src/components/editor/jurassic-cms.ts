import { atom } from 'jotai'
import * as EP from '../../core/shared/element-path'
import type { ElementPath } from '../../core/shared/project-file-types'
import { JURASSIC_CMS_URL } from '../../common/env-vars'

export type CMSUpdateStatus =
  | { type: 'updating'; value: string }
  | { type: 'error'; message: string }
  | { type: 'ok' }

export const CMSOptimisticValues_GLOBAL: {
  [elementPathString: string]: string
} = {}

interface CMSUpdateState {
  [elementPathString: string]: CMSUpdateStatus
}

export const CMSUpdateStateAtom = atom<CMSUpdateState>({})

export function setCMSUpdateStateForElementPath(
  elementPath: ElementPath,
  status: CMSUpdateStatus,
): (_: CMSUpdateState) => CMSUpdateState {
  if (status.type === 'updating') {
    CMSOptimisticValues_GLOBAL[EP.toString(elementPath)] = status.value
  }
  return (state: CMSUpdateState) => ({
    ...state,
    [EP.toString(elementPath)]: status,
  })
}

export function unsetCMSUpdateStateForElementPath(
  elementPath: ElementPath,
): (_: CMSUpdateState) => CMSUpdateState {
  delete CMSOptimisticValues_GLOBAL[EP.toString(elementPath)]
  return (state: CMSUpdateState) => {
    const nextState = { ...state }
    delete nextState[EP.toString(elementPath)]
    return nextState
  }
}

export async function updateJurassicCMS({
  project_id,
  key,
  updated,
}: {
  project_id: string
  key: string
  updated: string
}): Promise<void> {
  const response = await fetch(`http://${JURASSIC_CMS_URL}/api/${project_id}/${key}`, {
    method: 'POST',
    headers: new Headers({ 'content-type': 'application/json' }),
    body: JSON.stringify({ value: updated }),
    mode: 'no-cors',
  })
  if (!response.ok) {
    throw new Error(response.statusText)
  }
}
