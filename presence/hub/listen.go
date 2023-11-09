package hub

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"

	"github.com/concrete-utopia/flamingo/hub/messages"
	"github.com/concrete-utopia/flamingo/logging"
	"github.com/concrete-utopia/flamingo/types"
	"github.com/gobwas/ws/wsutil"
)

func (h *Hub) listen(handshake messages.MessageHandshake, conn net.Conn) error {
	defer conn.Close()

	room, ok := h.rooms.Get(handshake.RoomID)
	if !ok {
		return fmt.Errorf("room %q not found", handshake.RoomID)
	}
	for {
		select {
		case <-h.stopCh:
			return nil
		default:
			data, _, err := wsutil.ReadClientData(conn)
			if err == io.EOF || errors.As(err, &wsutil.ClosedError{}) {
				// disconnected
				return nil
			}
			if err != nil {
				return fmt.Errorf("read client data: %s", err)
			}

			room.Ping(handshake.PlayerID, handshake.PlayerName)

			request := messages.Request{}
			err = json.Unmarshal(data, &request)
			if err != nil {
				logging.Errorf("unmarshal message: %s", err)
				continue
			}

			switch request.Type {
			case messages.RequestMove:
				h.handleMove(room, handshake, request.Move)
			case messages.RequestSelection:
				h.handleSelection(room, handshake, request.Selection)
			case messages.RequestPing:
				h.handlePing(conn, room, handshake)
			default:
				logging.Warnf("unknown message action %q", request.Type)
			}
		}
	}
}

func (h *Hub) handleMove(room *types.Room, handshake messages.MessageHandshake, request *messages.MessageMove) {
	state := room.GetPlayerState(handshake.PlayerID)
	if request.Position != nil {
		state.Position = *request.Position
	}
	if request.CanvasOffset != nil {
		state.CanvasOffset = *request.CanvasOffset
	}
	if request.CanvasScale != nil {
		state.CanvasScale = *request.CanvasScale
	}

	room.UpdatePlayerState(handshake.PlayerID, state)
	move := *request
	move.PlayerID = &handshake.PlayerID

	err := h.broadcast(
		messages.NewOutboundMove(&move),
		handshake.RoomID, handshake.PlayerID)
	if err != nil {
		logging.Errorf("broadcast: %s", err)
	}
}

func (h *Hub) handleSelection(room *types.Room, handshake messages.MessageHandshake, request *messages.MessageSelection) {
	room.UpdateSelectedElements(handshake.PlayerID, request.Paths)
	err := h.broadcast(
		messages.NewOutboundState(room.GetState(), nil),
		handshake.RoomID, handshake.PlayerID)
	if err != nil {
		logging.Errorf("broadcast: %s", err)
	}
}

func (h *Hub) handlePing(conn net.Conn, room *types.Room, handshake messages.MessageHandshake) {
	err := h.sendToConn(conn,
		messages.NewOutboundState(room.GetState(), nil),
	)
	if err != nil {
		logging.Errorf("broadcast: %s", err)
	}
}
