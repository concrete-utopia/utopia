FROM haskell:8
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    curl \
    git \
    libghc-postgresql-libpq-dev
WORKDIR /usr/src/app/
COPY . .
WORKDIR /usr/src/app/server
RUN cabal update && cabal install --only-dependencies
CMD ["cabal", "install"]
