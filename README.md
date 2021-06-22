# ghoauth

CLI Tool to generate GitHub token by GitHub Apps

## Usage

```
$ ghoauth --help
ghoauth [options]
  -h  --help            Show this help text
      --version         Show version
  -v  --verbose         Enable verbose mode: verbosity level "debug"
      --client_id=TEXT  GitHub Apps client ID instead of CLIENT_ID environment variable
      --env-file=PATH   .env file path to write access token
      --env-var=TEXT    Environment variable name for access token
      --clip            Set user_code to clipboard
```

1. [Create GitHub Apps](https://github.com/settings/apps/new)
   - Enable "Expire user authorization tokens"
   - Enable "Only on this account"
2. Create `.env` file (default `~/.env`)
3. Run command with (1) client_id

```
ghoauth --client_id=CLIENT_ID --clip
```

## Build

```
stack build
```

### Docker

```
stack --docker --local-bin-path=./bin install
docker build -t matsubara0507/ghoauth . --build-arg local_bin_path=./bin
```
