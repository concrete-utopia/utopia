import React, { Component } from 'react'
import { BrowserRouter as Router, Route, Switch } from 'react-router-dom'
import { auth0Url } from './detect-env'
import { Projects, Featured } from './projects'
import { Settings } from './settings'
import { getAndClearRedirectUrl } from './common/persistence'
import { Policies } from './legal/policies'
import Jobs from './jobs'
import Team from './team'
import Prerelease from './prerelease'

class App extends Component {
  loginRender = () => {
    window.location.replace(auth0Url)
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
            <Route exact path='/' component={Jobs} />
            <Route exact path='/prerelease' component={Prerelease} />
            <Route exact path='/policies' component={Policies} />
            <Route exact path='/team' component={Team} />
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
