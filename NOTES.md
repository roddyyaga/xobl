## Missing from this implementation

### Xauth

Reference: https://gitlab.freedesktop.org/xorg/lib/libxau

- [ ] writing to the .Xauthority file (XauWriteAuth, XauLockAuth, XauUnlockAuth)

- [ ] handle auth schemes other than MIT-MAGIC-COOKIE-1
  (should we even? xgb doesn't)

### Connection

- [ ] use xmisc extension to look for unused xids

- [ ] try to connect to all connections returned by getaddrinfo
