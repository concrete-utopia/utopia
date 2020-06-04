export interface UserDetails {
  userId: string
  email?: string
  name?: string
  picture?: string
}

interface LoggedInUser {
  type: 'LOGGED_IN'
  user: UserDetails
}

interface NotLoggedIn {
  type: 'NOT_LOGGED_IN'
}

export type LoginState = LoggedInUser | NotLoggedIn

export function loggedInUser(user: UserDetails): LoggedInUser {
  return {
    type: 'LOGGED_IN',
    user: user,
  }
}

export const notLoggedIn: NotLoggedIn = {
  type: 'NOT_LOGGED_IN',
}

export function isLoggedIn(loginState: LoginState): loginState is LoggedInUser {
  return loginState.type === 'LOGGED_IN'
}
