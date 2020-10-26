## Missing from this implementation

### Xauth

Reference: https://gitlab.freedesktop.org/xorg/lib/libxau

- [ ] writing to the .Xauthority file (XauWriteAuth, XauLockAuth, XauUnlockAuth)

- [ ] handle auth schemes other than MIT-MAGIC-COOKIE-1
  (should we even? xgb doesn't)

### Display

- [ ] fully compliant display name parser
  there's some weird stuff about a slash in the src/xcb_util.c implementation
  in libxcb that I'll have to look into.
  xgb also handles it so we should.
  https://github.com/BurntSushi/xgb/blob/master/conn.go#L129
