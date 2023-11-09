package types

import (
	"sync"
	"time"

	"github.com/concrete-utopia/flamingo/logging"
)

type Rooms struct {
	data      map[string]*Room
	rmu       sync.RWMutex
	stopCh    chan struct{}
	clientTTL time.Duration
}

func NewRooms(clientTTL time.Duration) *Rooms {
	return &Rooms{
		data:      map[string]*Room{},
		stopCh:    make(chan struct{}),
		clientTTL: clientTTL,
	}
}

func (r *Rooms) GetOrCreate(id string) *Room {
	r.rmu.Lock()
	defer r.rmu.Unlock()

	_, ok := r.data[id]
	if !ok {
		r.data[id] = NewRoom(id, r.clientTTL)
		go r.data[id].Run()
	}
	return r.data[id]
}

func (r *Rooms) Get(id string) (*Room, bool) {
	r.rmu.RLock()
	defer r.rmu.RUnlock()

	room, ok := r.data[id]
	return room, ok
}

func (r *Rooms) Stop() {
	close(r.stopCh)
}

func (r *Rooms) Run() error {
	logging.Infof("running rooms")
	ticker := time.NewTicker(time.Minute)
	for {
		select {
		case <-r.stopCh:
			logging.Infof("stopping rooms")
			r.rmu.RLock()
			for _, r := range r.data {
				r.Stop()
			}
			r.rmu.RUnlock()
			return nil
		case <-ticker.C:
			// TODO cleanup rooms
		}
	}
}
