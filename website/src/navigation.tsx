/** @jsx jsx */
import * as React from 'react'
import { jsx } from '@emotion/react'
import { Global } from '@emotion/react'
import { breakpoints } from './home'

export default class Navigation extends React.Component {
  NavLink: React.FunctionComponent<{ emotion: Record<string, any>; href: string }> = (props) => {
    return (
      <a
        href={props.href}
        css={breakpoints({
          fontFamily: 'Work Sans, sans-serif',
          fontSize: [14, 22, 24],
          lineHeight: 1.4,
          fontWeight: 400,
          maxWidth: 800,
          marginLeft: ['1em', '2em', '2em'],
          marginRight: 'auto',
          textDecoration: 'none',
          flex: ['0 0 64px', '0 0 96px', '0 0 96px'],
          ...props.emotion,
        })}
      >
        {props.children}
      </a>
    )
  }

  render() {
    return (
      <React.Fragment>
        <Global
          styles={{
            body: {
              overflow: 'scroll',
              margin: 0,
            },
          }}
        />
        <nav
          css={breakpoints({
            maxWidth: [900, 900, 1200],
            margin: '0 auto',
            display: 'flex',
            justifyContent: 'space-between',
            alignItems: 'center',
            padding: '0 1rem',
          })}
        >
          <a
            className='logo'
            href='/prerelease'
            css={breakpoints({
              display: 'block',
              textDecoration: 'none',
              backgroundImage: [
                'url("/static/index/logo-smiangle-64x64@2x.png")',
                'url("/static/index/logo-smiangle-96x96@2x.png")',
                'url("/static/index/logo-smiangle-96x96@2x.png")',
              ],
              backgroundSize: '100%',
              width: [64, 96, 96],
              height: [64, 96, 96],
              flex: ['0 0 64px', '0 0 96px', '0 0 96px'],
            })}
          >
            &nbsp;
          </a>
          <div>
            <this.NavLink
              emotion={{
                '&, &:visited': {
                  color: '#444',
                },
              }}
              href='/prerelease#features'
            >
              Features
            </this.NavLink>
            <this.NavLink
              emotion={{
                '&, &:visited': {
                  color: '#444',
                },
              }}
              href='/team'
            >
              Team
            </this.NavLink>
            <this.NavLink
              emotion={{
                '&, &:visited': {
                  color: '#4524FB',
                },
              }}
              href='/login'
            >
              Prerelease Access
            </this.NavLink>
          </div>
        </nav>
      </React.Fragment>
    )
  }
}
