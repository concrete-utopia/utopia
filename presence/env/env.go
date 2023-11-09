package env

import "fmt"

type Environment string

const (
	Development Environment = "development"
	Production  Environment = "production"
)

var env Environment = Development

func Set(s string) {
	env = mustParse(s)
}

func Get() Environment {
	return env
}

func mustParse(s string) Environment {
	switch s {
	case string(Development):
		return Development
	case string(Production):
		return Production
	default:
		panic(fmt.Errorf("invalid environment %q", s))
	}
}

func IsProduction() bool {
	return env == Production
}

func IsDevelopment() bool {
	return env == Development
}
