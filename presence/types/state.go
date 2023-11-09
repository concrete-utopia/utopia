package types

import (
	"sync"
	"time"

	"github.com/concrete-utopia/flamingo/logging"
)

type PlayerState struct {
	ID           string  `json:"id"`
	Name         string  `json:"name"`
	Position     Coords  `json:"position"`
	CanvasScale  float64 `json:"canvasScale"`
	CanvasOffset Coords  `json:"canvasOffset"`
	Heartbeat    int64   `json:"-"`
}

func (s PlayerState) Alive(ttl time.Duration) bool {
	return time.Since(time.Unix(s.Heartbeat, 0)) < ttl
}

type RoomStateData struct {
	Players          []PlayerState `json:"players"`
	SelectedElements *[]string     `json:"selectedElements"`
}

func newRoomStateData() RoomStateData {
	return RoomStateData{}
}

type RoomState struct {
	rmu              sync.RWMutex
	players          map[string]*PlayerState
	selectedElements *[]string
}

func NewRoomState() *RoomState {
	return &RoomState{
		players: map[string]*PlayerState{},
	}
}

func (s *RoomState) AddPlayer(id string, data PlayerState) {
	s.rmu.Lock()
	defer s.rmu.Unlock()
	s.players[id] = &data
}

func (s *RoomState) Read() RoomStateData {
	s.rmu.RLock()
	defer s.rmu.RUnlock()
	data := newRoomStateData()
	for _, p := range s.players {
		data.Players = append(data.Players, *p)
	}
	if s.selectedElements != nil {
		selectedElements := make([]string, len(*s.selectedElements))
		copy(selectedElements, *s.selectedElements)
		data.SelectedElements = &selectedElements
	}
	return data
}

func (s *RoomState) Ping(playerID, name string) {
	s.rmu.Lock()
	defer s.rmu.Unlock()
	data, ok := s.players[playerID]
	if !ok {
		logging.Infof("resurrecting player %q", playerID)
		data = &PlayerState{
			ID:   playerID,
			Name: name,
		}
	}
	data.Heartbeat = time.Now().Unix()
	s.players[playerID] = data
}

func (s *RoomState) GetPlayerState(playerID string) *PlayerState {
	s.rmu.RLock()
	defer s.rmu.RUnlock()
	return s.players[playerID]
}

func (s *RoomState) UpdatePlayerState(playerID string, state *PlayerState) {
	s.rmu.Lock()
	defer s.rmu.Unlock()
	s.players[playerID] = state
}

func (s *RoomState) UpdateSelectedElements(playerID string, elements []string) {
	s.rmu.Lock()
	defer s.rmu.Unlock()
	s.selectedElements = &elements
}

func (s *RoomState) Remove(ids ...string) {
	s.rmu.Lock()
	defer s.rmu.Unlock()
	for _, id := range ids {
		logging.Infof("removing inactive %q", id)
		delete(s.players, id)
	}
}
