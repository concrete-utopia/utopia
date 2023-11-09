package hub

import (
	"encoding/json"
	"fmt"
	"strings"
	"sync"

	"github.com/concrete-utopia/flamingo/logging"
	"github.com/gobwas/ws/wsutil"
)

func (h *Hub) broadcast(data any, roomID string, skipIDs ...string) error {
	room, ok := h.rooms.Get(roomID)
	if !ok {
		return fmt.Errorf("room not found %s", roomID)
	}

	payload, err := json.Marshal(data)
	if err != nil {
		return fmt.Errorf("marshal data: %w", err)
	}

	conns := room.GetAllConns(skipIDs...)

	wg := sync.WaitGroup{}
	for id, conn := range conns {
		id := id
		conn := conn
		wg.Add(1)
		go func() {
			defer wg.Done()
			err := wsutil.WriteServerText(conn, payload)
			if err != nil {
				logging.Errorf("write server text: %s", err)
				if strings.Contains(err.Error(), "broken pipe") {
					room.RemoveConn(id)
				}
			}
		}()
	}
	wg.Wait()

	return nil
}
