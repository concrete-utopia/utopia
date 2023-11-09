package web

import (
	"fmt"
	"net/http"

	"github.com/concrete-utopia/flamingo/hub"
	"github.com/concrete-utopia/flamingo/logging"
	"github.com/gobwas/ws"
)

func NewServer(port int, hub *hub.Hub) http.Server {
	addr := fmt.Sprintf("0.0.0.0:%d", port)
	return http.Server{
		Addr:    addr,
		Handler: newHandler(hub),
	}
}

type handler struct {
	hub *hub.Hub
}

func newHandler(hub *hub.Hub) handler {
	return handler{
		hub: hub,
	}
}

func (h handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	upgrader := ws.DefaultHTTPUpgrader
	upgrader.Header = http.Header{}
	upgrader.Header.Set("Access-Control-Allow-Origin", "*")
	conn, _, _, err := upgrader.Upgrade(r, w)
	if err != nil {
		logging.Errorf("upgrade http: %w", err)
		return
	}
	err = h.hub.Handle(conn)
	if err != nil {
		logging.Errorf("handle connection: %s", err)
	}
}
