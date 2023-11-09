package hub

import (
	"encoding/json"
	"fmt"
	"net"
	"time"

	"github.com/concrete-utopia/flamingo/hub/messages"
	"github.com/concrete-utopia/flamingo/logging"
	"github.com/concrete-utopia/flamingo/types"
	"github.com/gobwas/ws/wsutil"
)

type Config struct {
	ClientTTL time.Duration
}

type Hub struct {
	conf   Config
	rooms  *types.Rooms
	stopCh chan struct{}
}

func New(conf Config) *Hub {
	return &Hub{
		conf:   conf,
		rooms:  types.NewRooms(conf.ClientTTL),
		stopCh: make(chan struct{}),
	}
}

func (h *Hub) Run() error {
	logging.Infof("running hub")

	return h.rooms.Run()
}

func (h *Hub) Stop() {
	close(h.stopCh)
}

func (h *Hub) Handle(conn net.Conn) error {
	// handshake and auth
	msg, _, err := wsutil.ReadClientData(conn)
	if err != nil {
		return fmt.Errorf("read client data: %w", err)
	}

	var request messages.Request
	err = json.Unmarshal(msg, &request)
	if err != nil {
		return fmt.Errorf("unmarshal handshake: %w", err)
	}
	if request.Type != messages.RequestHandshake {
		return fmt.Errorf("invalid handshake message")
	}
	if !request.Handshake.Valid() {
		return fmt.Errorf("invalid handshake data %+v %s", request.Handshake, string(msg))
	}

	room := h.rooms.GetOrCreate(request.Handshake.RoomID)
	room.Add(request.Handshake.PlayerID, request.Handshake.PlayerName, conn)

	err = h.sendToConn(conn, messages.NewOutboundState(room.GetState(), messages.NewReply(request.ID, true)))
	if err != nil {
		return fmt.Errorf("[room=%s player=%s] send reply: %w", request.Handshake.RoomID, request.Handshake.PlayerID, err)
	}

	go func() {
		err := h.listen(*request.Handshake, conn)
		if err != nil {
			logging.Errorf("[room=%s player=%s] listen: %s", request.Handshake.RoomID, request.Handshake.PlayerID, err)
		}
	}()

	return nil
}
