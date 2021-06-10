import { useRouter } from 'next/router'
import { getAndClearRedirectUrl } from '../components/common/persistence'

export default function Login() {
  const router = useRouter()
  // Make sure we're in the browser
  if (typeof window !== 'undefined') {
    getAndClearRedirectUrl().then((redirectUrl) => {
      router.push(redirectUrl)
    })
  }
  return null
}
