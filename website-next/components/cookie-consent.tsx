import CookieConsent, { Cookies, getCookieConsentValue } from 'react-cookie-consent'
import React from 'react'

declare function gtag(...args: any): void

function enableCookies() {
  gtag('consent', 'update', {
    analytics_storage: 'granted',
  })
}

export const CookieConsentBar = () => {
  if (getCookieConsentValue() === 'true') {
    enableCookies()
  }

  return (
    <CookieConsent
      location='bottom'
      style={{ background: '#383C4A', fontSize: '13px' }}
      buttonStyle={{
        color: '#383C4A',
        fontSize: '13px',
        borderRadius: 5,
        backgroundColor: '#00FC00',
      }}
      onAccept={enableCookies}
    >
      This website uses cookies to enhance the user experience.
    </CookieConsent>
  )
}
