import { BASE_URL } from './common/env-vars'

export const PRODUCTION_ENV: boolean = import.meta.env.NODE_ENV === 'production'

export const PRODUCTION_CONFIG: boolean =
  import.meta.env.SNOWPACK_PUBLIC_ENVIRONMENT_CONFIG === 'production'

const COMMIT_HASH = import.meta.env.SNOWPACK_PUBLIC_COMMIT_HASH || ''
export const URL_HASH = COMMIT_HASH === '' ? 'nocommit' : COMMIT_HASH

export const AUTH0_REDIRECT_URI: string = import.meta.env.SNOWPACK_PUBLIC_AUTH0_REDIRECT_URI || ''
export const AUTH0_CLIENT_ID: string = import.meta.env.SNOWPACK_PUBLIC_AUTH0_CLIENT_ID || ''
export const AUTH0_HOST: string = import.meta.env.SNOWPACK_PUBLIC_AUTH0_ENDPOINT || ''
const USE_AUTH0 = AUTH0_REDIRECT_URI != '' && AUTH0_CLIENT_ID != '' && AUTH0_HOST != ''

export const auth0Url = USE_AUTH0
  ? `https://${AUTH0_HOST}/authorize?scope=openid%20profile%20email&response_type=code&client_id=${AUTH0_CLIENT_ID}&redirect_uri=${AUTH0_REDIRECT_URI}`
  : `${BASE_URL(PRODUCTION_CONFIG)}authenticate?code=logmein`
