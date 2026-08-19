# Open an SSH tunnel to the CalCOFI server's PostgreSQL

Runs `ssh -N -L <local_port>:localhost:<remote_port> <ssh_host>` in the
background with `processx`, so
[`cc_pg_connect()`](https://calcofi.io/calcofi4r/reference/cc_pg_connect.md)
can reach the database on `localhost:<local_port>`. Uses your
`~/.ssh/config` entry (the docs call it `calcofi`:
`HostName ssh.calcofi.io`, `User <you>`, `IdentityFile …`), so no
credentials are handled here. Windows 10+ has `ssh.exe` built in.

## Usage

``` r
cc_pg_tunnel(
  ssh_host = "calcofi",
  local_port = 5432L,
  remote_port = 5432L,
  wait = 10
)

cc_pg_tunnel_close(ssh_host = "calcofi", local_port = 5432L)
```

## Arguments

- ssh_host:

  the `Host` alias from `~/.ssh/config` (or `user@ssh.calcofi.io`)

- local_port:

  local port to listen on; default `5432`

- remote_port:

  port on the server; default `5432`

- wait:

  seconds to wait for the port to come up; default `10`

## Value

the
[`processx::process`](http://processx.r-lib.org/reference/process.md)
(invisibly), or `NULL` if the port was already open

## Details

The process is kept in a package-level registry and reused while alive;
call `cc_pg_tunnel_close()` to stop it. If something already listens on
`local_port` (an earlier tunnel, a local Postgres) the port is left
alone and a message says so — set `local_port = 15432` in both places in
that case.
