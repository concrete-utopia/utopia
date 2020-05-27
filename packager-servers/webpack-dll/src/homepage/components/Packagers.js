import React from 'react';
import Packager from './Packager';

export default class Packagers extends React.PureComponent {
  state = {
    connected: false,
    data: {},
  };

  componentDidMount() {
    this.setupSocket();
  }

  setupSocket = () => {
    this.socket = new WebSocket('wss://webpack-dll-prod.herokuapp.com');

    this.socket.onmessage = ev => {
      try {
        this.setState({
          connected: true,
          data: { ...this.state.data, ...JSON.parse(ev.data) },
        });
      } catch (e) {
        console.error(e);
      }
    };

    this.socket.onopen = ev => {
      this.setState({ connected: true });
    };

    this.socket.onclose = ev => {
      this.setState({ connected: false }, () => {
        this.setupSocket();
      });
    };
  };

  render() {
    const { connected, data } = this.state;

    if (!connected)
      return <div style={{ textAlign: 'center' }}>Connecting...</div>;

    return (
      <div
        style={{
          display: 'flex',
          justifyContent: 'space-around',
          marginBottom: '5rem',
          flexWrap: 'wrap',
        }}
      >
        {Object.keys(data).sort().map((packagerIp, index) => {
          const packager = data[packagerIp];

          return (
            <Packager
              key={packagerIp}
              name={`Packager ${index}`}
              isAvailable={packager.isAvailable}
              lastUsed={packager.lastUsed}
              isBusy={packager.isBusyCount > 0}
              resolvedCount={packager.resolvedCount}
              errorCount={packager.errorCount}
            />
          );
        })}
      </div>
    );
  }
}
