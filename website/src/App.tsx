import React, { Component } from 'react'
import { BrowserRouter as Router, Route, Switch } from 'react-router-dom'
import { auth0Url } from './common/env-vars'
import { Preview } from './preview'
import { Projects, Featured } from './projects'
import { Settings } from './settings'
import { getAndClearRedirectUrl } from './common/persistence'
import { Policies } from './legal/policies'
import Home from './home'
import Team from './team'
import Prerelease from './prerelease'

var PlaceholderScreen = () => {
  return (
    <div>
      <div
        style={{
          height: '40px',
          position: 'sticky',
          top: 0,
          backgroundColor: 'white',
        }}
        className='flex flex-row items-center justify-center '
      />
      <div style={{ height: '90vh' }} className='flex flex-row items-center justify-center '>
        <img
          src='/static/logotype-exactsized-brandpurple-420x93.svg'
          alt='Utopia. Welcome to the future :) '
        />
      </div>
    </div>
  )
}

class App extends Component {
  loginRender = () => {
    window.location.replace(auth0Url('redirect'))
    return null
  }

  logoutRender = () => {
    window.location.replace('/logout')
    return null
  }

  authdRender = () => {
    getAndClearRedirectUrl().then((redirectUrl) => {
      window.location.replace(redirectUrl)
    })
    return null
  }

  render() {
    return (
      <Router>
        <div style={{ height: '100%' }}>
          <Switch>
            <Route exact path='/' component={Home} />
            <Route exact path='/prerelease' component={Prerelease} />
            <Route exact path='/policies' component={Policies} />
            <Route exact path='/team' component={Team} />
            <Route exact path='/preview/:id' component={Preview} />
            <Route exact path='/projects' component={Projects} />
            <Route exact path='/featured' component={Featured} />
            <Route exact path='/settings' component={Settings} />
            <Route exact path='/login' render={this.loginRender} />
            <Route exact path='/logout' render={this.logoutRender} />
            <Route exact path='/authd' render={this.authdRender} />
          </Switch>
        </div>
      </Router>
    )
  }
}
export default App
