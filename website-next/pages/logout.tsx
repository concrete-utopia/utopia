import { useRouter } from 'next/router'

export default function Login() {
  const router = useRouter()
  // Make sure we're in the browser
  if (typeof window !== 'undefined') {
    router.push('/logout')
  }
  return null
}
