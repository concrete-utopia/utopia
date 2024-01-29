import { v4 as UUID } from 'uuid'
import { IS_TEST_ENVIRONMENT, UTOPIA_BACKEND } from '../../common/env-vars'
import { HEADERS, MODE } from '../../common/server'
import type { EditorDispatch } from './action-types'
import { showToast } from './actions/action-creators'
import { notice } from '../common/notice'

export interface ClaimProjectControl {
  type: 'CLAIM_PROJECT_CONTROL'
  projectID: string
  collaborationEditor: string
}

function claimProjectControl(projectID: string, collaborationEditor: string): ClaimProjectControl {
  return {
    type: 'CLAIM_PROJECT_CONTROL',
    projectID: projectID,
    collaborationEditor: collaborationEditor,
  }
}

export interface SnatchProjectControl {
  type: 'SNATCH_PROJECT_CONTROL'
  projectID: string
  collaborationEditor: string
}

function snatchProjectControl(
  projectID: string,
  collaborationEditor: string,
): SnatchProjectControl {
  return {
    type: 'SNATCH_PROJECT_CONTROL',
    projectID: projectID,
    collaborationEditor: collaborationEditor,
  }
}

export interface ReleaseProjectControl {
  type: 'RELEASE_PROJECT_CONTROL'
  projectID: string
  collaborationEditor: string
}

function releaseProjectControl(
  projectID: string,
  collaborationEditor: string,
): ReleaseProjectControl {
  return {
    type: 'RELEASE_PROJECT_CONTROL',
    projectID: projectID,
    collaborationEditor: collaborationEditor,
  }
}

export interface ClearAllOfCollaboratorsControl {
  type: 'CLEAR_ALL_OF_COLLABORATORS_CONTROL'
  collaborationEditor: string
}

function clearAllOfCollaboratorsControl(
  collaborationEditor: string,
): ClearAllOfCollaboratorsControl {
  return {
    type: 'CLEAR_ALL_OF_COLLABORATORS_CONTROL',
    collaborationEditor: collaborationEditor,
  }
}

export type CollaborationRequest =
  | ClaimProjectControl
  | SnatchProjectControl
  | ReleaseProjectControl
  | ClearAllOfCollaboratorsControl

export interface RequestProjectControlResult {
  type: 'REQUEST_PROJECT_CONTROL_RESULT'
  successfullyClaimed: boolean
}

export interface ReleaseControlResponse {
  type: 'RELEASE_CONTROL_RESULT'
}

export type CollaborationResponse = RequestProjectControlResult | ReleaseControlResponse

const collaborationEditor = UUID()

async function callCollaborationEndpoint(
  request: CollaborationRequest,
): Promise<CollaborationResponse> {
  const url = `${UTOPIA_BACKEND}collaboration`
  const response = await fetch(url, {
    method: 'PUT',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
    body: JSON.stringify(request),
  })
  if (response.ok) {
    return response.json()
  } else {
    throw new Error(`server responded with ${response.status} ${response.statusText}`)
  }
}

async function claimControlOverProject(projectID: string | null): Promise<boolean | null> {
  if (IS_TEST_ENVIRONMENT) {
    return true
  }
  if (projectID == null) {
    return null
  }

  const request = claimProjectControl(projectID, collaborationEditor)
  const response = await callCollaborationEndpoint(request)
  if (response.type === 'REQUEST_PROJECT_CONTROL_RESULT') {
    return response.successfullyClaimed
  } else {
    throw new Error(`Unexpected response: ${JSON.stringify(response)}`)
  }
}

async function snatchControlOverProject(projectID: string | null): Promise<boolean | null> {
  if (IS_TEST_ENVIRONMENT) {
    return true
  }
  if (projectID == null) {
    return null
  }

  const request = snatchProjectControl(projectID, collaborationEditor)
  const response = await callCollaborationEndpoint(request)
  if (response.type === 'REQUEST_PROJECT_CONTROL_RESULT') {
    return response.successfullyClaimed
  } else {
    throw new Error(`Unexpected response: ${JSON.stringify(response)}`)
  }
}

async function releaseControlOverProject(projectID: string | null): Promise<void> {
  if (IS_TEST_ENVIRONMENT) {
    return
  }
  if (projectID == null) {
    return
  }

  const request = releaseProjectControl(projectID, collaborationEditor)
  const response = await callCollaborationEndpoint(request)
  if (response.type !== 'RELEASE_CONTROL_RESULT') {
    throw new Error(`Unexpected response: ${JSON.stringify(response)}`)
  }
}

function displayControlErrorToast(dispatch: EditorDispatch, message: string): void {
  dispatch([showToast(notice(message, 'ERROR', false, 'control-error'))])
}

async function clearAllControlFromThisEditor(): Promise<void> {
  if (IS_TEST_ENVIRONMENT) {
    return
  }
  const request = clearAllOfCollaboratorsControl(collaborationEditor)

  const response = await callCollaborationEndpoint(request)
  if (response.type !== 'RELEASE_CONTROL_RESULT') {
    throw new Error(`Unexpected response: ${JSON.stringify(response)}`)
  }
}

export const CollaborationEndpoints = {
  snatchControlOverProject: snatchControlOverProject,
  claimControlOverProject: claimControlOverProject,
  releaseControlOverProject: releaseControlOverProject,
  clearAllControlFromThisEditor: clearAllControlFromThisEditor,
  displayControlErrorToast: displayControlErrorToast,
}
