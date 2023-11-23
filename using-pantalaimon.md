# How to use encrypted room #

Ement.el doesn't support encrypted rooms natively, but it can be used transparently with the E2EE-aware reverse proxy daemon [Pantalaimon](https://github.com/matrix-org/pantalaimon/).

1. [Install Pantalaimon](#Install-Pantalaimon)
2. [Configure Pantalaimon](#Configure-Pantalaimon)
3. [Verify the Pantalaimon session](#Verify-the-Pantalaimon-session)
4. [Troubleshooting](#Troubleshooting)

# Install Pantalaimon #

## Arch Linux

There is a [pantalaimon-git](https://aur.archlinux.org/packages/pantalaimon-git) package in AUR.

```console
$ yay -S pantalaimon-git
```

## Alpine Linux

```
$ apk add pantalaimon pantalaimon-ui # Not tested
```

## Other

See https://github.com/matrix-org/pantalaimon/blob/master/README.md#installation

```console
$ pantalaimon --version
pantalaimon, version 0.10.5
```

# Configure Pantalaimon #

Create ta file in `~/.config/pantalaimon/pantalaimon.conf` with this contents

```conf
[local-matrix]
# Should be replaced with the server your are using if it isn’t matrix.org
Homeserver = https://matrix.org
ListenAddress = localhost
ListenPort = 8010
SSL = False
UseKeyring = False
IgnoreVerification = True
```

Then start the pantalaimon server.

```console
$ pantalaimon
======== Starting daemon for homeserver local-matrix on http://127.0.0.1:8010 ========
(Press CTRL+C to quit)
```

Alternatively, you can also use the `--log-level debug` option to see what is going on if something doesn’t work.

```console
$ pantalaimon --log-level debug
```

Now if you open http://localhost:8010/ you should see the https://matrix.org/ web page. If you get a error about `ssl`, go [here](#cannot-connect-to-host-matrixorg-443-ssldefault).

## Systemd service

You will probably want to start it through a service/daemon, the Arch Linux AUR package add this [one](https://aur.archlinux.org/cgit/aur.git/tree/pantalaimon.service?h=pantalaimon-git).

Otherwise you can create `~/.config/systemd/user/pantalaimon.service` with this contents :

```conf
[Unit]
Description=pantalaimon daemon
After=network.target

[Service]
Restart=on-failure
RestartSec=3
ExecStart=/usr/bin/pantalaimon

[Install]
WantedBy=default.target
```

- Start the service : `systemctl --user start pantalaimon.service`
- See if it is running : `systemctl --user status pantalaimon.service`
- Stop the service : `systemctl --user stop pantalaimon.service`
- Enable the service : `systemctl --user enable pantalaimon.service`

# Verify the Pantalaimon session #

## Connect through Pantalaimon ##

Add this code in your Emacs configuration

```emacs-lisp
(defun my-ement-panta-connect ()
  (interactive)
  (ement-connect :uri-prefix "http://localhost:8010" :user-id "@your-identifiant:matrix.org"))
```

Then, do a `M-x my-ement-panta-connect`, choose "password", write your Matrix password, and connect.

You can control your Pantalaimon server by using `panctl` [command](https://github.com/matrix-org/pantalaimon/blob/master/docs/man/panctl.md).
If you get an error about DBus here, see : [g-dbus-error-quark](#g-dbus-error-quark)

```console
$ panctl
panctl> list-servers
pantalaimon servers:
 - Name: local-matrix
 - Pan users:
   - @your-identifiant:matrix.org RCXONVDBKV
```

(if the `list-servers` doesn't return anything, it means you aren’t connected with Ement.el, you should connect first)

## Verify Pantalaimon ##

On Element-Desktop :
1. Connect to your account
2. Open the "Settings" page
3. Look at your current Session ID (it should be something like "DWOCPKJVTK")

```console
$ panctl
panctl> start-verification @your-identifiant:matrix.org @your-identifiant:matrix.org DWOCPKJVTK
Successfully started the key verification request
Short authentication string for pan user @your-identifiant:matrix.org from @your-identifiant:matrix.org via DWOCPKJVTK:
     ⚽          ✈️           🚀          🐶          🐎          👍          📕
    Ball      Airplane     Rocket       Dog        Horse     Thumbs up      Book
```

Confirm that they match on Element-Desktop

```console
panctl> confirm-verification @your-identifiant:matrix.org @your-identifiant:matrix.org
Device DWOCPKJVTK of user @your-identifiant:matrix.org successfully verified for pan user @your-identifiant:matrix.org.
```

## Import your rooms keys ##

You will need to get your E2E rooms keys.

On Element-Desktop :
1. Connect to your account
2. Open the "Settings" page
3. Go on the "Security & Privacy" tab
4. Click on "Export E2E room keys"
5. Choose a passphrase (you will need to remember it), and save the `element-keys.txt` file.

```console
panctl> import-keys @your-identifiant:matrix.org element-keys.txt your-passphrase
Successfully imported keys for @your-identifiant:matrix.org from element-keys.txt
```

- You need to disconnect from Ement.el : `M-x ement-disconnect`
- Reconnect : `M-x my-ement-panta-connect`
- Enjoy :-)

# Troubleshooting #


- https://www.cogitri.dev/posts/10-pantalaimon-setup/
- https://github.com/alphapapa/ement.el/issues/153
- https://github.com/alphapapa/ement.el/pull/232


## Cannot connect to host matrix.org :443 ssl:default ##

If you see this error when you open http://localhost:8010/ :

`Cannot connect to host matrix.org :443 ssl:default [Name or service not known]`


or this one with ement.el :

```lisp
Debugger entered--Lisp error: (ement-api-error "500: nil")
  signal(ement-api-error ("500: nil"))
  ement-api-error(#s(plz-error :curl-error nil :response #s(plz-response :version 1.1 :status 500 :headers ((content-type . "text/plain; charset=utf-8") (content-length . "78") (date . "Thu, 23 Nov 2023 02:01:25 GMT") (server . "Python/3.11 aiohttp/3.8.4")) :body "Cannot connect to host matrix.org :443 ssl:default [Name or service not known]") :message nil))
  plz--respond(#<process plz-request-curl> #<buffer  *plz-request-curl*> "finished\n")
  apply(plz--respond (#<process plz-request-curl> #<buffer  *plz-request-curl*> "finished\n"))
```

It probably mean there is a problem in your `~/.config/pantalaimon/pantalaimon.conf`, in particular there should not be comment at the end of line.

```conf
[local-matrix]
Homeserver = https://matrix.org # /!\ this end of line comment will make your configuration fail /!\
```

## g-dbus-error-quark ##

If you get this error :

`g-dbus-error-quark: GDBus.Error:org.freedesktop.DBus.Error.ServiceUnknown: The name org.pantalaimon1 was not provided by any .service files`

Your are maybe lacking the `python-notify2` librairy.
