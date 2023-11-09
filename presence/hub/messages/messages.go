package messages

import "github.com/concrete-utopia/flamingo/types"

type MessageMove struct {
	PlayerID     *string       `json:"playerId,omitempty"`
	Position     *types.Coords `json:"position"`
	CanvasScale  *float64      `json:"canvasScale"`
	CanvasOffset *types.Coords `json:"canvasOffset"`
}

type MessageSelection struct {
	Paths []string `json:"paths"`
}

type MessageHandshake struct {
	PlayerID   string `json:"playerId"`
	PlayerName string `json:"playerName"`
	RoomID     string `json:"roomId"`
}

func (h MessageHandshake) Valid() bool {
	return h.PlayerID != "" && h.PlayerName != "" && h.RoomID != ""
}

type requestType string

const (
	RequestMove      requestType = "move"
	RequestSelection requestType = "selection"
	RequestHandshake requestType = "handshake"
	RequestPing      requestType = "ping"
)

type Request struct {
	ID        string            `json:"id"`
	Reply     bool              `json:"reply"`
	Type      requestType       `json:"type"`
	Move      *MessageMove      `json:"move,omitempty"`
	Selection *MessageSelection `json:"selection,omitempty"`
	Handshake *MessageHandshake `json:"handshake,omitempty"`
}

type outboundType string

const (
	outboundState outboundType = "state"
	outboundMove  outboundType = "move"
	outboundReply outboundType = "reply"
)

type Outbound struct {
	Type  outboundType         `json:"type"`
	Reply *reply               `json:"reply,omitempty"`
	State *types.RoomStateData `json:"state,omitempty"`
	Move  *MessageMove         `json:"move,omitempty"`
}

func NewOutboundState(state types.RoomStateData, reply *reply) *Outbound {
	return &Outbound{
		Type:  outboundState,
		State: &state,
		Reply: reply,
	}
}

func NewOutboundMove(move *MessageMove) *Outbound {
	return &Outbound{
		Type: outboundMove,
		Move: move,
	}
}

type reply struct {
	RequestID string `json:"requestId"`
	Ok        bool   `json:"ok"`
}

func NewReply(requestID string, ok bool) *reply {
	return &reply{
		RequestID: requestID,
		Ok:        ok,
	}
}
