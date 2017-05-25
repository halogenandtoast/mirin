# Mirin

A tiny redirection server using Spock backed by MySQL

## Setup

* Install MySQL
* Install Stack: [Instructions](https://docs.haskellstack.org/en/stable/README/)
* Install OpenSSL: (e.g. `brew install openssl`)
* Run setup: `bin/setup`

## Running

* Build the app: `stack build`
* Run the server: `stack exec mirin`


## Docker

You can use `docker-compose`:

* docker-compose build
* docker-compose up app
