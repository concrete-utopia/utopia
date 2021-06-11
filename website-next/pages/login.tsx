import { useRouter } from 'next/router'
import { auth0Url } from '../components/common/env-vars'

export default function Login() {
  const router = useRouter()
  // Make sure we're in the browser
  if (typeof window !== 'undefined') {
    router.push(auth0Url('redirect'))
  }
  return null
}
