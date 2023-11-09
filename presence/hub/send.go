package hub

import (
	"encoding/json"
	"fmt"
	"net"

	"github.com/concrete-utopia/flamingo/hub/messages"
	"github.com/gobwas/ws/wsutil"
)

func (h *Hub) sendToConn(conn net.Conn, data *messages.Outbound) error {
	payload, err := json.Marshal(data)
	if err != nil {
		return fmt.Errorf("marshal data: %w", err)
	}

	err = wsutil.WriteServerText(conn, payload)
	if err != nil {
		return fmt.Errorf("write server text: %w", err)
	}

	return nil
}
