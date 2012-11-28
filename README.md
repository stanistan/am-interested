# am-interested

A bittorrent client in Clojure.

## Usage

None yet.

## Notes

### Flow

- Open torrent file
  - Are we alrady downloading/uploading this?
    (status, where do we store metadata)
  - Parse torrent data
  - Collect current info (if already in process)
  - Connect to tracker (send stats)
     - Get a list of peers (normalized)

### Structure

- `Torrent` type
  - will have fns to spawn threads for peers and keep their status
- `Peer` type (sub: `Seed|Leach`) communicating over tcp (socklet ns)

### Settings

- how much do we seed (ratio)?
- max numbers of peers/seeds/leachers
- max speeds

## License

Copyright © 2012 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
