package types

import (
	"net"
	"sync"
)

type Conns struct {
	rmu  sync.RWMutex
	data map[string]net.Conn
}

func NewConns() *Conns {
	return &Conns{
		data: map[string]net.Conn{},
	}
}

func (c *Conns) Add(id string, conn net.Conn) {
	c.rmu.Lock()
	defer c.rmu.Unlock()
	old, ok := c.data[id]
	if ok {
		_ = old.Close() // ignore the error, we don't care about it
	}
	c.data[id] = conn
}

func (c *Conns) GetAll(skipIDs ...string) map[string]net.Conn {
	c.rmu.RLock()
	defer c.rmu.RUnlock()
	withSkipped := map[string]net.Conn{}
	for k, v := range c.data {
		skip := false
		for _, skipID := range skipIDs {
			if k == skipID {
				skip = true
				break
			}
		}
		if !skip {
			withSkipped[k] = v
		}
	}
	return withSkipped
}

func (c *Conns) Get(id string) (net.Conn, bool) {
	c.rmu.RLock()
	defer c.rmu.RUnlock()
	conn, ok := c.data[id]
	return conn, ok
}

func (c *Conns) Remove(id string) {
	c.rmu.Lock()
	defer c.rmu.Unlock()
	delete(c.data, id)
}
