package logging

import (
	"fmt"
	"strings"

	"github.com/concrete-utopia/flamingo/env"
	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"
)

var logger *zap.Logger
var sugar *zap.SugaredLogger

func ParseLevel(s string) zapcore.Level {
	switch strings.ToLower(s) {
	case "debug":
		return zapcore.DebugLevel
	case "info":
		return zapcore.InfoLevel
	case "warn":
		return zapcore.WarnLevel
	case "error":
		return zapcore.ErrorLevel
	default:
		panic(fmt.Errorf("invalid log level %q", s))
	}
}

func Init(level zapcore.Level) {
	var config zap.Config
	if env.IsProduction() {
		config = zap.NewProductionConfig()
	} else {
		config = zap.NewDevelopmentConfig()
	}
	config.Level = zap.NewAtomicLevelAt(level)
	logger, _ = config.Build()
	sugar = logger.Sugar()
}

func With(fields ...zap.Field) *zap.SugaredLogger {
	return logger.With(fields...).Sugar()
}

func Infof(s string, args ...interface{}) {
	sugar.Infof(s, args...)
}

func Errorf(s string, args ...interface{}) {
	sugar.Errorf(s, args...)
}

func Warnf(s string, args ...interface{}) {
	sugar.Warnf(s, args...)
}

func Fatalf(s string, args ...interface{}) {
	sugar.Fatalf(s, args...)
}
