package main

import (
	"net/http"
	"os"
	"os/signal"
	"time"

	"github.com/concrete-utopia/flamingo/env"
	"github.com/concrete-utopia/flamingo/hub"
	"github.com/concrete-utopia/flamingo/logging"
	"github.com/concrete-utopia/flamingo/web"
	"github.com/urfave/cli"
)

var config struct {
	environment string
	logLevel    string
	http        struct {
		port int
	}
	hub struct {
		clientTTL time.Duration
	}
}

func main() {
	app := cli.NewApp()
	app.Name = "Presence Server"
	app.Usage = "Keepin' it real" // FIXME
	app.Flags = []cli.Flag{
		cli.IntFlag{
			Name:        "http-port",
			EnvVar:      "HTTP_PORT",
			Destination: &config.http.port,
			Value:       8080,
			Usage:       "The port to listen to for HTTP traffic",
		},
		cli.DurationFlag{
			Name:        "hub-client-ttl",
			EnvVar:      "HUB_CLIENT_TTL",
			Destination: &config.hub.clientTTL,
			Value:       time.Minute,
			Usage:       "TTL for connected clients",
		},
		cli.StringFlag{
			Name:        "environment",
			EnvVar:      "ENVIRONMENT",
			Destination: &config.environment,
			Value:       string(env.Development),
			Usage:       "The current environment (production, development, ...)",
		},
		cli.StringFlag{
			Name:        "log-level",
			EnvVar:      "LOG_LEVEL",
			Destination: &config.logLevel,
			Value:       "info",
			Usage:       "The logging level",
		},
	}
	app.Action = run
	err := app.Run(os.Args)
	if err != nil {
		logging.Fatalf("run: %s", err)
	}
}

func run(c *cli.Context) error {
	env.Set(config.environment)
	logging.Init(logging.ParseLevel(config.logLevel))

	hub := hub.New(hub.Config{
		ClientTTL: config.hub.clientTTL,
	})
	go func() {
		err := hub.Run()
		if err != nil {
			logging.Errorf("hub run: %s", err)
		}
	}()

	server := web.NewServer(config.http.port, hub)
	go func() {
		err := server.ListenAndServe()
		if err != nil && err != http.ErrServerClosed {
			logging.Errorf("listen: %s", err)
		}
	}()
	defer server.Close()

	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, os.Interrupt)
	sig := <-sigCh
	logging.Infof("terminating (%s)", sig)

	return nil
}
