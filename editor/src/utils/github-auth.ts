import { UTOPIA_BACKEND } from '../common/env-vars'
import { MODE } from '../common/server'
import urljoin from 'url-join'
import type { LoginState } from '../common/user'
import type { EditorDispatch } from '../components/editor/action-types'
import { setGithubState } from '../components/editor/actions/action-creators'
import { updateUserDetailsWhenAuthenticated } from '../core/shared/github/helpers'

async function checkIfAuthenticatedWithGithub(): Promise<boolean> {
  const url = urljoin(UTOPIA_BACKEND, 'github', 'authentication', 'status')
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

export async function isAuthenticatedWithGithub(loginState: LoginState): Promise<boolean> {
  if (loginState.type === 'LOGGED_IN') {
    return checkIfAuthenticatedWithGithub()
  } else {
    return false
  }
}

const timeToWait = 1000 * 5

export async function startGithubAuthentication(dispatch: EditorDispatch): Promise<void> {
  // Open the window that starts the authentication flow.
  const url = urljoin(UTOPIA_BACKEND, 'github', 'authentication', 'start')
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
