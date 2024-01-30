import { IS_TEST_ENVIRONMENT, UTOPIA_BACKEND } from '../../common/env-vars'
import { HEADERS, MODE } from '../../common/server'
import { cachedPromise } from '../../core/shared/promise-utils'

let isLiveblocksEnabledOnServer: boolean = false

export async function checkLiveblocksEnabledOnServer(): Promise<void> {
  if (IS_TEST_ENVIRONMENT) {
    isLiveblocksEnabledOnServer = false
  } else {
    return cachedPromise('liveblocks-enabled', async () => {
      try {
        const response = await fetch(`${UTOPIA_BACKEND}liveblocks/enabled`, {
          method: 'GET',
          credentials: 'include',
          headers: HEADERS,
          mode: MODE,
        })
        if (response.ok) {
          const jsonResponse = await response.json()
          if (typeof jsonResponse === 'boolean') {
            isLiveblocksEnabledOnServer = jsonResponse
          } else {
            console.error(
              `Invalid response body when checking Liveblocks availability: ${JSON.stringify(
                jsonResponse,
              )}`,
            )
          }
        } else {
          console.error(
            `Unexpected response when checking Liveblocks availability (${response.status}): ${response.statusText}`,
          )
        }
      } catch (error) {
        console.error('Error response when checking Liveblocks availability.', error)
      }
    })
  }
}

export function isLiveblocksEnabled(): boolean {
  return isLiveblocksEnabledOnServer
}
