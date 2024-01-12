import * as React from 'react'
import { MODE } from '../common/server'
import type { LoginState } from '../common/user'
import type { EditorDispatch } from '../components/editor/action-types'
import { setGithubState } from '../components/editor/actions/action-creators'
import { useDispatch } from '../components/editor/store/dispatch-context'
import { GithubEndpoints } from '../core/shared/github/endpoints'
import { updateUserDetailsWhenAuthenticated } from '../core/shared/github/helpers'

async function checkIfAuthenticatedWithGithub(): Promise<boolean> {
  const url = GithubEndpoints.authenticationStatus()
  const response = await fetch(url, {
    method: 'GET',
    credentials: 'include',
    mode: MODE,
  })
  if (response.ok) {
    return response.json()
  } else {
    throw new Error(`Github: Unexpected status returned from endpoint: ${response.status}`)
  }
}

async function isAuthenticatedWithGithub(loginState: LoginState): Promise<boolean> {
  if (loginState.type === 'LOGGED_IN') {
    return checkIfAuthenticatedWithGithub()
  } else {
    return false
  }
}

const timeToWait = 1000 * 5

async function startGithubAuthentication(dispatch: EditorDispatch): Promise<void> {
  // Open the window that starts the authentication flow.
  const url = GithubEndpoints.authenticationStart()
  window.open(url)

  async function checkAuthenticatedPeriodically(timeLeftMS: number): Promise<void> {
    const currentStatus = await updateUserDetailsWhenAuthenticated(
      dispatch,
      checkIfAuthenticatedWithGithub(),
    )
    if (currentStatus) {
      dispatch([setGithubState({ authenticated: true })], 'everyone')
    } else {
      if (timeLeftMS > 0) {
        // Wait for a bit and try again.
        const wait = new Promise((resolve) => {
          setTimeout(resolve, timeToWait)
        })
        return wait.then(() => {
          return checkAuthenticatedPeriodically(timeLeftMS - timeToWait)
        })
      } else {
        console.error('Timeout waiting for Github authentication to succeed.')
      }
    }
  }

  // Try this for a maximum of 5 minutes.
  await checkAuthenticatedPeriodically(1000 * 60 * 5)
}

export const GithubAuth = {
  isAuthenticatedWithGithub: isAuthenticatedWithGithub,
  startGithubAuthentication: startGithubAuthentication,
} as const
