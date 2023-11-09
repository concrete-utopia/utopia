package types

import (
	"net"
	"time"

	"github.com/concrete-utopia/flamingo/logging"
	"go.uber.org/zap"
)

type Room struct {
	id        string
	state     *RoomState
	conns     *Conns
	stopCh    chan struct{}
	clientTTL time.Duration
}

func NewRoom(id string, clientTTL time.Duration) *Room {
	return &Room{
		id:        id,
		state:     NewRoomState(),
		conns:     NewConns(),
		stopCh:    make(chan struct{}),
		clientTTL: clientTTL,
	}
}

func (r *Room) Stop() {
	close(r.stopCh)
}

func (r *Room) Run() {
	logging.Infof("running room %q", r.id)
	ticker := time.NewTicker(time.Second)
	for {
		select {
		case <-r.stopCh:
			logging.Infof("stopping room %q", r.id)
			return
		case <-ticker.C:
			state := r.state.Read()
			dead := make([]string, 0, len(state.Players))
			for _, p := range state.Players {
				alive := p.Alive(r.clientTTL)
				logging.With(
					zap.String("room", r.id),
					zap.String("id", p.ID),
					zap.String("name", p.Name),
					zap.Int64("hb", p.Heartbeat),
					zap.Bool("alive", alive),
				).Debugf("player")
				if !alive {
					dead = append(dead, p.ID)
				}
			}
			if len(dead) > 0 {
				r.state.Remove(dead...)
			}
		}
	}
}

func (r *Room) Add(playerID, playerName string, conn net.Conn) {
	r.conns.Add(playerID, conn)
	r.state.AddPlayer(playerID, PlayerState{
		ID:        playerID,
		Name:      playerName,
		Heartbeat: time.Now().Unix(),
	})
}

func (r *Room) GetAllConns(skipIDs ...string) map[string]net.Conn {
	return r.conns.GetAll(skipIDs...)
}

func (r *Room) GetConn(id string) (net.Conn, bool) {
	return r.conns.Get(id)
}

func (r *Room) RemoveConn(id string) {
	r.conns.Remove(id)
}

func (r *Room) Ping(playerID, name string) {
	r.state.Ping(playerID, name)
}

func (r *Room) GetPlayerState(playerID string) *PlayerState {
	return r.state.GetPlayerState(playerID)
}

func (r *Room) UpdatePlayerState(playerID string, state *PlayerState) {
	// TODO this should return the state here
	r.state.UpdatePlayerState(playerID, state)
}

func (r *Room) UpdateSelectedElements(playerID string, elements []string) {
	// TODO this should return the state here
	r.state.UpdateSelectedElements(playerID, elements)
}

func (r *Room) GetState() RoomStateData {
	return r.state.Read()
}
