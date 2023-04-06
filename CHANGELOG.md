# Changelog


## 1.0.2.4

### Changed

- Allow base 4.17, 4.18 (GHC 9.4, 9.6).
- Allow text<2.1

## 1.0.2.2

## 1.0.2.3

Support GHC 9.2.1.

### Changed

- Allow base 4.16 (GHC 9.2.1).


## 1.0.2.2

Windows support.

### Changed

- Fix `utf8-troubleshoot` on Windows.


## 1.0.2.1

A technical clean up release.

### Changed

- Specify missing version bounds for dependencies.


## 1.0.2.0

Improve `utf8-troubleshoot` to make it useful for identifying tricky cases.

### Changed

- `utf8-troubleshoot`: improve available locale detection
- `utf8-troubleshoot`: display raw results from C libraries


## 1.0.1.0

GHC 8.10 compatibility and a new troubleshooting tool.

### Added

- `utf8-troubleshoot` – the troubleshooting tool

### Changed

- Bump `base` for GHC 8.10


## 1.0.0.0

Initial release.

### Added

- `withUtf8`
- `withStdTerminalHandles`
- `setHandleEncoding`
- `withHandle`
- `setTerminalHandleEncoding`
- `withTerminalHandle`
- `openFile`
- `withFile`
- `readFile`
- `writeFile`
