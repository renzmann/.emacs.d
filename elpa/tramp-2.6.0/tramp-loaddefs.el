;;; tramp-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "tramp" "tramp.el" (0 0 0 0))
;;; Generated autoloads from tramp.el

(defvar tramp--startup-hook nil "\
Forms to be executed at the end of tramp.el.")

(put 'tramp--startup-hook 'tramp-suppress-trace t)

(defmacro tramp--with-startup (&rest body) "\
Schedule BODY to be executed at the end of tramp.el." `(add-hook 'tramp--startup-hook (lambda nil ,@body)))

(defvar tramp-verbose 3 "\
Verbosity level for Tramp messages.
Any level x includes messages for all levels 1 .. x-1.  The levels are

 0  silent (no tramp messages at all)
 1  errors
 2  warnings
 3  connection to remote hosts (default level)
 4  activities
 5  internal
 6  sent and received strings
 7  connection properties
 8  file caching
 9  test commands
10  traces (huge)
11  call traces (maintainer only).")

(custom-autoload 'tramp-verbose "tramp" t)

(defconst tramp-system-name (or (system-name) "") "\
The system name Tramp is running locally.")

(defvar tramp-methods nil "\
Alist of methods for remote files.
This is a list of entries of the form (NAME PARAM1 PARAM2 ...).
Each NAME stands for a remote access method.  Each PARAM is a
pair of the form (KEY VALUE).  The following KEYs are defined:

  * `tramp-remote-shell'
    This specifies the shell to use on the remote host.  This
    MUST be a Bourne-like shell.  It is normally not necessary to
    set this to any value other than \"/bin/sh\": Tramp wants to
    use a shell which groks tilde expansion, but it can search
    for it.  Also note that \"/bin/sh\" exists on all Unixen
    except Andtoid, this might not be true for the value that you
    decide to use.  You Have Been Warned.

  * `tramp-remote-shell-login'
    This specifies the arguments to let `tramp-remote-shell' run
    as a login shell.  It defaults to (\"-l\"), but some shells,
    like ksh, require another argument.  See
    `tramp-connection-properties' for a way to overwrite the
    default value.

  * `tramp-remote-shell-args'
    For implementation of `shell-command', this specifies the
    arguments to let `tramp-remote-shell' run a single command.

  * `tramp-login-program'
    This specifies the name of the program to use for logging in to the
    remote host.  This may be the name of rsh or a workalike program,
    or the name of telnet or a workalike, or the name of su or a workalike.

  * `tramp-login-args'
    This specifies a list of lists of arguments to pass to the
    above mentioned program.  You normally want to put each
    argument in an individual string, i.e.
    (\"-a\" \"-b\") rather than (\"-a -b\").

    \"%\" followed by a letter are expanded in the arguments as
    follows:

    - \"%h\" is replaced by the host name
    - \"%u\" is replaced by the user name
    - \"%p\" is replaced by the port number
    - \"%%\" can be used to obtain a literal percent character.

    If a sub-list containing \"%h\", \"%u\" or \"%p\" is
    unchanged after expansion (i.e. no host, no user or no port
    were specified), that sublist is not used.  For e.g.

    \\='((\"-a\" \"-b\") (\"-l\" \"%u\"))

    that means that (\"-l\" \"%u\") is used only if the user was
    specified, and it is thus effectively optional.

    Other expansions are:

    - \"%l\" is replaced by the login shell `tramp-remote-shell'
      and its parameters.
    - \"%t\" is replaced by the temporary file name produced with
      `tramp-make-tramp-temp-file'.
    - \"%k\" indicates the keep-date parameter of a program, if exists.
    - \"%c\" adds additional `tramp-ssh-controlmaster-options'
      options for the first hop.
    - \"%n\" expands to \"2>/dev/null\".
    - \"%x\" is replaced by the `tramp-scp-strict-file-name-checking'
      argument if it is supported.
    - \"%y\" is replaced by the `tramp-scp-force-scp-protocol'
      argument if it is supported.
    - \"%z\" is replaced by the `tramp-scp-direct-remote-copying'
      argument if it is supported.
    - \"%d\" is replaced by the device detected by `tramp-adb-get-device'.

    The existence of `tramp-login-args', combined with the
    absence of `tramp-copy-args', is an indication that the
    method is capable of multi-hops.

  * `tramp-async-args'
    When an asynchronous process is started, we know already that
    the connection works.  Therefore, we can pass additional
    parameters to suppress diagnostic messages, in order not to
    tamper the process output.

  * `tramp-direct-async'
    Whether the method supports direct asynchronous processes.
    Until now, just \"ssh\"-based, \"sshfs\"-based, \"adb\"-based
    and container methods do.  If it is a list of strings, they
    are used to construct the remote command.

  * `tramp-config-check'
    A function to be called with one argument, VEC.  It should
    return a string which is used to check, whether the
    configuration of the remote host has been changed (which
    would require to flush the cache data).  This string is kept
    as connection property \"config-check-data\".

  * `tramp-copy-program'
    This specifies the name of the program to use for remotely copying
    the file; this might be the absolute filename of scp or the name of
    a workalike program.  It is always applied on the local host.

  * `tramp-copy-args'
    This specifies the list of parameters to pass to the above mentioned
    program, the hints for `tramp-login-args' also apply here.

  * `tramp-copy-env'
     A list of environment variables and their values, which will
     be set when calling `tramp-copy-program'.

  * `tramp-remote-copy-program'
    The listener program to be applied on remote side, if needed.

  * `tramp-remote-copy-args'
    The list of parameters to pass to the listener program, the hints
    for `tramp-login-args' also apply here.  Additionally, \"%r\" could
    be used here and in `tramp-copy-args'.  It denotes a randomly
    chosen port for the remote listener.

  * `tramp-copy-keep-date'
    This specifies whether the copying program when the preserves the
    timestamp of the original file.

  * `tramp-copy-keep-tmpfile'
    This specifies whether a temporary local file shall be kept
    for optimization reasons (useful for \"rsync\" methods).

  * `tramp-copy-recursive'
    Whether the operation copies directories recursively.

  * `tramp-default-port'
    The default port of a method.

  * `tramp-tmpdir'
    A directory on the remote host for temporary files.  If not
    specified, \"/tmp\" is taken as default.

  * `tramp-connection-timeout'
    This is the maximum time to be spent for establishing a connection.
    In general, the global default value shall be used, but for
    some methods, like \"doas\", \"su\" or \"sudo\", a shorter
    timeout might be desirable.

  * `tramp-session-timeout'
    How long a Tramp connection keeps open before being disconnected.
    This is useful for methods like \"doas\" or \"sudo\", which
    shouldn't run an open connection in the background forever.

  * `tramp-password-previous-hop'
    The password for this connection is the same like the
    password for the previous hop.  If there is no previous hop,
    the password of the local user is applied.  This is needed
    for methods like \"doas\", \"sudo\" or \"sudoedit\".

  * `tramp-case-insensitive'
    Whether the remote file system handles file names case insensitive.
    Only a non-nil value counts, the default value nil means to
    perform further checks on the remote host.  See
    `tramp-connection-properties' for a way to overwrite this.

  * `tramp-mount-args'
  * `tramp-copyto-args'
  * `tramp-moveto-args'
  * `tramp-about-args'
    These parameters, a list of list like `tramp-login-args', are used
    for the \"rclone\" method, and are appended to the respective
    \"rclone\" commands.  In general, they shouldn't be changed inside
    `tramp-methods'; it is recommended to change their values via
    `tramp-connection-properties'.  Unlike `tramp-login-args' there is
     no pattern replacement.

What does all this mean?  Well, you should specify `tramp-login-program'
for all methods; this program is used to log in to the remote site.  Then,
there are two ways to actually transfer the files between the local and the
remote side.  One way is using an additional scp-like program.  If you want
to do this, set `tramp-copy-program' in the method.

Another possibility for file transfer is inline transfer, i.e. the
file is passed through the same buffer used by `tramp-login-program'.  In
this case, the file contents need to be protected since the
`tramp-login-program' might use escape codes or the connection might not
be eight-bit clean.  Therefore, file contents are encoded for transit.
See the variables `tramp-local-coding-commands' and
`tramp-remote-coding-commands' for details.

So, to summarize: if the method is an out-of-band method, then you
must specify `tramp-copy-program' and `tramp-copy-args'.  If it is an
inline method, then these two parameters should be nil.

Notes:

All these arguments can be overwritten by connection properties.
See Info node `(tramp) Predefined connection information'.

When using `su', `sudo' or `doas' the phrase \"open connection to
a remote host\" sounds strange, but it is used nevertheless, for
consistency.  No connection is opened to a remote host, but `su',
`sudo' or `doas' is started on the local host.  You should
specify a remote host `localhost' or the name of the local host.
Another host name is useful only in combination with
`tramp-default-proxies-alist'.")

(defvar tramp-default-method-alist nil "\
Default method to use for specific host/user pairs.
This is an alist of items (HOST USER METHOD).  The first matching item
specifies the method to use for a file name which does not specify a
method.  HOST and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-method' takes effect.

If the file name does not specify the user, lookup is done using the
empty string for the user name.

See `tramp-methods' for a list of possibilities for METHOD.")

(custom-autoload 'tramp-default-method-alist "tramp" t)

(defvar tramp-default-user-alist nil "\
Default user to use for specific method/host pairs.
This is an alist of items (METHOD HOST USER).  The first matching item
specifies the user to use for a file name which does not specify a
user.  METHOD and HOST are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-user' takes effect.

If the file name does not specify the method, lookup is done using the
empty string for the method name.")

(custom-autoload 'tramp-default-user-alist "tramp" t)

(defvar tramp-default-host-alist nil "\
Default host to use for specific method/user pairs.
This is an alist of items (METHOD USER HOST).  The first matching item
specifies the host to use for a file name which does not specify a
host.  METHOD and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-host' takes effect.

If the file name does not specify the method, lookup is done using the
empty string for the method name.")

(custom-autoload 'tramp-default-host-alist "tramp" t)

(defvar tramp-local-host-regexp (tramp-compat-rx bos (| (literal tramp-system-name) (| "localhost" "localhost4" "localhost6" "127.0.0.1" "::1")) eos) "\
Host names which are regarded as local host.
If the local host runs a chrooted environment, set this to nil.")

(custom-autoload 'tramp-local-host-regexp "tramp" t)

(defvar tramp-terminal-type "dumb" "\
Value of TERM environment variable for logging in to remote host.
Because Tramp wants to parse the output of the remote shell, it is easily
confused by ANSI color escape sequences and suchlike.  Often, shell init
files conditionalize this setup based on the TERM environment variable.")

(custom-autoload 'tramp-terminal-type "tramp" t)

(defconst tramp-root-id-string "root" "\
String used to denote the root user or group.")
(require 'cl-lib)

(cl-defstruct (tramp-file-name (:type list) :named) method user domain host port localname hop)

(autoload 'tramp-file-name-unify "tramp" "\
Unify VEC by removing localname and hop from `tramp-file-name' structure.
If LOCALNAME is an absolute file name, set it as localname.  If
LOCALNAME is a relative file name, return `tramp-cache-undefined'.
Objects returned by this function compare `equal' if they refer to the
same connection.  Make a copy in order to avoid side effects.

\(fn VEC &optional LOCALNAME)" nil nil)

(autoload 'tramp-tramp-file-p "tramp" "\
Return t if NAME is a string with Tramp file name syntax.

\(fn NAME)" nil nil)

(autoload 'tramp-file-local-name "tramp" "\
Return the local name component of NAME.
This function removes from NAME the specification of the remote
host and the method of accessing the host, leaving only the part
that identifies NAME locally on the remote system.  If NAME does
not match `tramp-file-name-regexp', just `file-local-name' is
called.  The returned file name can be used directly as argument
of `process-file', `start-file-process', or `shell-command'.

\(fn NAME)" nil nil)

(autoload 'tramp-dissect-file-name "tramp" "\
Return a `tramp-file-name' structure of NAME, a remote file name.
The structure consists of method, user, domain, host, port,
localname (file name on remote host), and hop.

Unless NODEFAULT is non-nil, method, user and host are expanded
to their default values.  For the other file name parts, no
default values are used.

\(fn NAME &optional NODEFAULT)" nil nil)

(autoload 'tramp-ensure-dissected-file-name "tramp" "\
Return a `tramp-file-name' structure for VEC-OR-FILENAME.

VEC-OR-FILENAME may be either a string or a `tramp-file-name'.
If it's not a Tramp filename, return nil.

\(fn VEC-OR-FILENAME)" nil nil)

(autoload 'tramp-make-tramp-file-name "tramp" "\
Construct a Tramp file name from ARGS.

ARGS could have two different signatures.  The first one is of
type (VEC &optional LOCALNAME).
If LOCALNAME is nil, the value in VEC is used.  If it is a
symbol, a null localname will be used.  Otherwise, LOCALNAME is
expected to be a string, which will be used.

The other signature exists for backward compatibility.  It has
the form (METHOD USER DOMAIN HOST PORT LOCALNAME &optional HOP).

\(fn &rest ARGS)" nil nil)

(autoload 'tramp-get-connection-buffer "tramp" "\
Get the connection buffer to be used for VEC.
Unless DONT-CREATE, the buffer is created when it doesn't exist yet.
In case a second asynchronous communication has been started, it is different
from `tramp-get-buffer'.

\(fn VEC &optional DONT-CREATE)" nil nil)

(defsubst tramp-get-buffer-string (&optional buffer) "\
Return contents of BUFFER.
If BUFFER is not a buffer or a buffer name, return the contents
of `current-buffer'." (with-current-buffer (or buffer (current-buffer)) (substring-no-properties (buffer-string))))

(autoload 'tramp-debug-message "tramp" "\
Append message to debug buffer of VEC.
Message is formatted with FMT-STRING as control string and the remaining
ARGUMENTS to actually emit the message (if applicable).

\(fn VEC FMT-STRING &rest ARGUMENTS)" nil nil)

(defvar tramp-inhibit-progress-reporter nil "\
Show Tramp progress reporter in the minibuffer.
This variable is used to disable concurrent progress reporter messages.")

(defsubst tramp-message (vec-or-proc level fmt-string &rest arguments) "\
Emit a message depending on verbosity level.
VEC-OR-PROC identifies the Tramp buffer to use.  It can be either a
vector or a process.  LEVEL says to be quiet if `tramp-verbose' is
less than LEVEL.  The message is emitted only if `tramp-verbose' is
greater than or equal to LEVEL.

The message is also logged into the debug buffer when `tramp-verbose'
is greater than or equal 4.

Calls functions `message' and `tramp-debug-message' with FMT-STRING as
control string and the remaining ARGUMENTS to actually emit the message (if
applicable)." (ignore-errors (when (<= level tramp-verbose) (when (and (<= level 3) (null tramp-inhibit-progress-reporter)) (apply #'message (concat (cond ((= level 0) "") ((= level 1) "") ((= level 2) "Warning: ") (t "Tramp: ")) fmt-string) arguments)) (when (>= tramp-verbose 4) (let ((tramp-verbose 0)) (when (= level 1) (ignore-errors (setq fmt-string (concat fmt-string "
%s") arguments (append arguments `(,(tramp-get-buffer-string (if (processp vec-or-proc) (process-buffer vec-or-proc) (tramp-get-connection-buffer vec-or-proc 'dont-create)))))))) (when (processp vec-or-proc) (setq vec-or-proc (process-get vec-or-proc 'vector)))) (when (tramp-file-name-p vec-or-proc) (apply #'tramp-debug-message vec-or-proc (concat (format "(%d) # " level) fmt-string) arguments))))))

(autoload 'tramp-set-completion-function "tramp" "\
Set the list of completion functions for METHOD.
FUNCTION-LIST is a list of entries of the form (FUNCTION FILE).
The FUNCTION is intended to parse FILE according its syntax.
It might be a predefined FUNCTION, or a user defined FUNCTION.
For the list of predefined FUNCTIONs see `tramp-completion-function-alist'.

Example:

    (tramp-set-completion-function
     \"ssh\"
     \\='((tramp-parse-sconfig \"/etc/ssh_config\")
       (tramp-parse-sconfig \"~/.ssh/config\")))

\(fn METHOD FUNCTION-LIST)" nil nil)

(autoload 'tramp-register-foreign-file-name-handler "tramp" "\
Register (FUNC . HANDLER) in `tramp-foreign-file-name-handler-alist'.
FUNC is the function, which takes a dissected filename and determines
whether HANDLER is to be called.  Add operations defined in
`HANDLER-alist' to `tramp-file-name-handler'.

\(fn FUNC HANDLER &optional APPEND)" nil nil)

;;;***

;;;### (autoloads nil "tramp-adb" "tramp-adb.el" (0 0 0 0))
;;; Generated autoloads from tramp-adb.el

(defvar tramp-adb-program "adb" "\
Name of the Android Debug Bridge program.")

(custom-autoload 'tramp-adb-program "tramp-adb" t)

(defconst tramp-adb-method "adb" "\
When this method name is used, forward all calls to Android Debug Bridge.")

(tramp--with-startup (add-to-list 'tramp-methods `(,tramp-adb-method (tramp-login-program ,tramp-adb-program) (tramp-login-args (("-s" "%d") ("shell"))) (tramp-direct-async t) (tramp-tmpdir "/data/local/tmp") (tramp-default-port 5555))) (add-to-list 'tramp-default-host-alist `(,tramp-adb-method nil "")) (tramp-set-completion-function tramp-adb-method '((tramp-adb-parse-device-names ""))))

(defconst tramp-adb-file-name-handler-alist '((access-file . tramp-handle-access-file) (add-name-to-file . tramp-handle-add-name-to-file) (copy-directory . tramp-handle-copy-directory) (copy-file . tramp-adb-handle-copy-file) (delete-directory . tramp-adb-handle-delete-directory) (delete-file . tramp-adb-handle-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-handle-directory-files) (directory-files-and-attributes . tramp-adb-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . tramp-adb-handle-exec-path) (expand-file-name . tramp-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . tramp-adb-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-adb-handle-file-executable-p) (file-exists-p . tramp-adb-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-adb-handle-file-local-copy) (file-locked-p . tramp-handle-file-locked-p) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-adb-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-adb-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-adb-handle-file-system-info) (file-truename . tramp-handle-file-truename) (file-writable-p . tramp-adb-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (list-system-processes . tramp-handle-list-system-processes) (load . tramp-handle-load) (lock-file . tramp-handle-lock-file) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-adb-handle-make-directory) (make-directory-internal . ignore) (make-lock-file-name . tramp-handle-make-lock-file-name) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . tramp-adb-handle-make-process) (make-symbolic-link . tramp-handle-make-symbolic-link) (memory-info . tramp-handle-memory-info) (process-attributes . tramp-handle-process-attributes) (process-file . tramp-adb-handle-process-file) (rename-file . tramp-adb-handle-rename-file) (set-file-acl . ignore) (set-file-modes . tramp-adb-handle-set-file-modes) (set-file-selinux-context . ignore) (set-file-times . tramp-adb-handle-set-file-times) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . tramp-handle-shell-command) (start-file-process . tramp-handle-start-file-process) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-home-directory . ignore) (tramp-get-remote-gid . tramp-adb-handle-get-remote-gid) (tramp-get-remote-groups . tramp-adb-handle-get-remote-groups) (tramp-get-remote-uid . tramp-adb-handle-get-remote-uid) (tramp-set-file-uid-gid . ignore) (unhandled-file-name-directory . ignore) (unlock-file . tramp-handle-unlock-file) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-adb-handle-write-region)) "\
Alist of handler functions for Tramp ADB method.")

(defsubst tramp-adb-file-name-p (vec-or-filename) "\
Check if it's a VEC-OR-FILENAME for ADB." (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename))) (string= (tramp-file-name-method vec) tramp-adb-method)))

(autoload 'tramp-adb-file-name-handler "tramp-adb" "\
Invoke the ADB handler for OPERATION.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(tramp--with-startup (tramp-register-foreign-file-name-handler #'tramp-adb-file-name-p #'tramp-adb-file-name-handler))

(autoload 'tramp-adb-parse-device-names "tramp-adb" "\
Return a list of (nil host) tuples allowed to access.

\(fn IGNORE)" nil nil)

;;;***

;;;### (autoloads nil "tramp-archive" "tramp-archive.el" (0 0 0 0))
;;; Generated autoloads from tramp-archive.el

(defconst tramp-archive-file-name-regexp (eval-when-compile (ignore-errors (tramp-archive-autoload-file-name-regexp))) "\
Regular expression matching archive file names.")

(defconst tramp-archive-method "archive" "\
Method name for archives in GVFS.")

(defconst tramp-archive-file-name-handler-alist '((access-file . tramp-archive-handle-access-file) (add-name-to-file . tramp-archive-handle-not-implemented) (copy-file . tramp-archive-handle-copy-file) (delete-directory . tramp-archive-handle-not-implemented) (delete-file . tramp-archive-handle-not-implemented) (directory-file-name . tramp-archive-handle-directory-file-name) (directory-files . tramp-archive-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . tramp-archive-handle-not-implemented) (dired-uncache . tramp-archive-handle-dired-uncache) (exec-path . ignore) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . tramp-archive-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-archive-handle-file-executable-p) (file-exists-p . tramp-archive-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-archive-handle-file-local-copy) (file-locked-p . ignore) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-archive-handle-file-name-all-completions) (file-name-case-insensitive-p . ignore) (file-name-completion . tramp-handle-file-name-completion) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . ignore) (file-notify-rm-watch . ignore) (file-notify-valid-p . ignore) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-archive-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-archive-handle-file-system-info) (file-truename . tramp-archive-handle-file-truename) (file-writable-p . ignore) (find-backup-file-name . ignore) (insert-directory . tramp-archive-handle-insert-directory) (insert-file-contents . tramp-archive-handle-insert-file-contents) (list-system-processes . ignore) (load . tramp-archive-handle-load) (lock-file . ignore) (make-auto-save-file-name . ignore) (make-directory . tramp-archive-handle-not-implemented) (make-directory-internal . tramp-archive-handle-not-implemented) (make-lock-file-name . ignore) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . ignore) (make-symbolic-link . tramp-archive-handle-not-implemented) (memory-info . ignore) (process-attributes . ignore) (process-file . ignore) (rename-file . tramp-archive-handle-not-implemented) (set-file-acl . ignore) (set-file-modes . tramp-archive-handle-not-implemented) (set-file-selinux-context . ignore) (set-file-times . tramp-archive-handle-not-implemented) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . tramp-archive-handle-not-implemented) (start-file-process . tramp-archive-handle-not-implemented) (temporary-file-directory . tramp-archive-handle-temporary-file-directory) (tramp-get-home-directory . ignore) (tramp-get-remote-gid . ignore) (tramp-get-remote-groups . ignore) (tramp-get-remote-uid . ignore) (tramp-set-file-uid-gid . ignore) (unhandled-file-name-directory . ignore) (unlock-file . ignore) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-archive-handle-not-implemented)) "\
Alist of handler functions for file archive method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defun tramp-archive-run-real-handler (operation args) "\
Invoke normal file name handler for OPERATION.
First arg specifies the OPERATION, second arg ARGS is a list of
arguments to pass to the OPERATION." (let* ((inhibit-file-name-handlers `(tramp-archive-file-name-handler \, (and (eq inhibit-file-name-operation operation) inhibit-file-name-handlers))) (inhibit-file-name-operation operation)) (apply operation args)))

(autoload 'tramp-archive-file-name-handler "tramp-archive" "\
Invoke the file archive related OPERATION.
First arg specifies the OPERATION, second arg ARGS is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

;;;***

;;;### (autoloads nil "tramp-cache" "tramp-cache.el" (0 0 0 0))
;;; Generated autoloads from tramp-cache.el

(defvar tramp-cache-data (make-hash-table :test #'equal) "\
Hash table for remote files properties.")

(defvar tramp-connection-properties nil "\
List of static connection properties.
Every entry has the form (REGEXP PROPERTY VALUE).  The regexp
matches remote file names.  It can be nil.  PROPERTY is a string,
and VALUE the corresponding value.  They are used, if there is no
matching entry for PROPERTY in `tramp-cache-data'.  For more
details see the info pages.")

(custom-autoload 'tramp-connection-properties "tramp-cache" t)

(defvar tramp-persistency-file-name (locate-user-emacs-file "tramp") "\
File which keeps connection history for Tramp connections.")

(custom-autoload 'tramp-persistency-file-name "tramp-cache" t)

(defconst tramp-cache-version (make-tramp-file-name :method "cache") "\
Virtual connection vector for Tramp version.")

(defconst tramp-cache-undefined 'undef "\
The symbol marking undefined hash keys and values.")

(autoload 'tramp-get-file-property "tramp-cache" "\
Get the PROPERTY of FILE from the cache context of KEY.
Return DEFAULT if not set.

\(fn KEY FILE PROPERTY &optional DEFAULT)" nil nil)

(autoload 'tramp-set-file-property "tramp-cache" "\
Set the PROPERTY of FILE to VALUE, in the cache context of KEY.
Return VALUE.

\(fn KEY FILE PROPERTY VALUE)" nil nil)

(autoload 'tramp-file-property-p "tramp-cache" "\
Check whether PROPERTY of FILE is defined in the cache context of KEY.

\(fn KEY FILE PROPERTY)" nil nil)

(autoload 'tramp-flush-file-property "tramp-cache" "\
Remove PROPERTY of FILE in the cache context of KEY.

\(fn KEY FILE PROPERTY)" nil nil)

(autoload 'tramp-flush-file-properties "tramp-cache" "\
Remove all properties of FILE in the cache context of KEY.

\(fn KEY FILE)" nil nil)

(autoload 'tramp-flush-directory-properties "tramp-cache" "\
Remove all properties of DIRECTORY in the cache context of KEY.
Remove also properties of all files in subdirectories.

\(fn KEY DIRECTORY)" nil nil)

(autoload 'tramp-flush-file-function "tramp-cache" "\
Flush all Tramp cache properties from `buffer-file-name'.
This is suppressed for temporary buffers." nil nil)

(autoload 'with-tramp-file-property "tramp-cache" "\
Check in Tramp cache for PROPERTY, otherwise execute BODY and set cache.
FILE must be a local file name on a connection identified via KEY.

\(fn KEY FILE PROPERTY &rest BODY)" nil t)

(function-put 'with-tramp-file-property 'lisp-indent-function '3)

(autoload 'with-tramp-saved-file-property "tramp-cache" "\
Save PROPERTY, run BODY, reset PROPERTY.
Preserve timestamps.

\(fn KEY FILE PROPERTY &rest BODY)" nil t)

(function-put 'with-tramp-saved-file-property 'lisp-indent-function '3)

(autoload 'with-tramp-saved-file-properties "tramp-cache" "\
Save PROPERTIES, run BODY, reset PROPERTIES.
PROPERTIES is a list of file properties (strings).
Preserve timestamps.

\(fn KEY FILE PROPERTIES &rest BODY)" nil t)

(function-put 'with-tramp-saved-file-properties 'lisp-indent-function '3)

(autoload 'tramp-get-connection-property "tramp-cache" "\
Get the named PROPERTY for the connection.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.
If KEY is `tramp-cache-undefined', or if the value is not set for
the connection, return DEFAULT.

\(fn KEY PROPERTY &optional DEFAULT)" nil nil)

(autoload 'tramp-set-connection-property "tramp-cache" "\
Set the named PROPERTY of a connection to VALUE.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.  If KEY
is `tramp-cache-undefined', nothing is set.
PROPERTY is set persistent when KEY is a `tramp-file-name' structure.
Return VALUE.

\(fn KEY PROPERTY VALUE)" nil nil)

(autoload 'tramp-connection-property-p "tramp-cache" "\
Check whether named PROPERTY of a connection is defined.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.

\(fn KEY PROPERTY)" nil nil)

(autoload 'tramp-flush-connection-property "tramp-cache" "\
Remove the named PROPERTY of a connection identified by KEY.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.
PROPERTY is set persistent when KEY is a `tramp-file-name' structure.

\(fn KEY PROPERTY)" nil nil)

(autoload 'tramp-flush-connection-properties "tramp-cache" "\
Remove all properties identified by KEY.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.

\(fn KEY)" nil nil)

(autoload 'with-tramp-connection-property "tramp-cache" "\
Check in Tramp for property PROPERTY, otherwise execute BODY and set.

\(fn KEY PROPERTY &rest BODY)" nil t)

(function-put 'with-tramp-connection-property 'lisp-indent-function '2)

(autoload 'with-tramp-saved-connection-property "tramp-cache" "\
Save PROPERTY, run BODY, reset PROPERTY.

\(fn KEY PROPERTY &rest BODY)" nil t)

(function-put 'with-tramp-saved-connection-property 'lisp-indent-function '2)

(autoload 'with-tramp-saved-connection-properties "tramp-cache" "\
Save PROPERTIES, run BODY, reset PROPERTIES.
PROPERTIES is a list of file properties (strings).

\(fn KEY PROPERTIES &rest BODY)" nil t)

(function-put 'with-tramp-saved-connection-properties 'lisp-indent-function '2)

(autoload 'tramp-cache-print "tramp-cache" "\
Print hash table TABLE.

\(fn TABLE)" nil nil)

(autoload 'tramp-list-connections "tramp-cache" "\
Return all active `tramp-file-name' structs according to `tramp-cache-data'." nil nil)

(defvar tramp-completion-use-cache t "\
Whether to use the Tramp cache for completion of user and host names.
Set it to nil if there are invalid entries in the cache, for
example if the host configuration changes often, or if you plug
your laptop to different networks frequently.")

(custom-autoload 'tramp-completion-use-cache "tramp-cache" t)

(autoload 'tramp-parse-connection-properties "tramp-cache" "\
Return a list of (user host) tuples allowed to access for METHOD.
This function is added always in `tramp-get-completion-function'
for all methods.  Resulting data are derived from connection history.

\(fn METHOD)" nil nil)

(defvar tramp-cache-read-persistent-data (or init-file-user site-run-file) "\
Whether to read persistent data at startup time.")

;;;***

;;;### (autoloads nil "tramp-cmds" "tramp-cmds.el" (0 0 0 0))
;;; Generated autoloads from tramp-cmds.el

(autoload 'tramp-change-syntax "tramp-cmds" "\
Change Tramp syntax.
SYNTAX can be one of the symbols `default' (default),
`simplified' (ange-ftp like) or `separate' (XEmacs like).

\(fn &optional SYNTAX)" t nil)

(autoload 'tramp-list-tramp-buffers "tramp-cmds" "\
Return a list of all Tramp connection buffers." nil nil)

(autoload 'tramp-list-remote-buffers "tramp-cmds" "\
Return a list of all buffers with remote `default-directory'." nil nil)

(defvar tramp-cleanup-connection-hook nil "\
List of functions to be called after Tramp connection is cleaned up.
Each function is called with the current vector as argument.")

(autoload 'tramp-cleanup-connection "tramp-cmds" "\
Flush all connection related objects.
This includes password cache, file cache, connection cache,
buffers, processes.  KEEP-DEBUG non-nil preserves the debug
buffer.  KEEP-PASSWORD non-nil preserves the password cache.
KEEP-PROCESSES non-nil preserves the asynchronous processes.
When called interactively, a Tramp connection has to be selected.

\(fn VEC &optional KEEP-DEBUG KEEP-PASSWORD KEEP-PROCESSES)" t nil)

(autoload 'tramp-cleanup-this-connection "tramp-cmds" "\
Flush all connection related objects of the current buffer's connection." t nil)

(function-put #'tramp-cleanup-this-connection 'completion-predicate #'tramp-command-completion-p)

(defvar tramp-cleanup-all-connections-hook nil "\
List of functions to be called after all Tramp connections are cleaned up.")

(autoload 'tramp-cleanup-all-connections "tramp-cmds" "\
Flush all Tramp internal objects.
This includes password cache, file cache, connection cache, buffers." t nil)

(autoload 'tramp-cleanup-all-buffers "tramp-cmds" "\
Kill all remote buffers." t nil)

(autoload 'tramp-rename-files "tramp-cmds" "\
Replace in all buffers the visiting file name from SOURCE to TARGET.
SOURCE is a remote directory name, which could contain also a
localname part.  TARGET is the directory name SOURCE is replaced
with.  Often, TARGET is a remote directory name on another host,
but it can also be a local directory name.  If TARGET has no
local part, the local part from SOURCE is used.

If TARGET is nil, it is selected according to the first match in
`tramp-default-rename-alist'.  If called interactively, this
match is offered as initial value for selection.

On all buffers, which have a `buffer-file-name' matching SOURCE,
this name is modified by replacing SOURCE with TARGET.  This is
applied by calling `set-visited-file-name'.  The new
`buffer-file-name' is prompted for modification in the
minibuffer.  The buffers are marked modified, and must be saved
explicitly.

If user option `tramp-confirm-rename-file-names' is nil, changing
the file name happens without confirmation.  This requires a
matching entry in `tramp-default-rename-alist'.

Remote buffers related to the remote connection identified by
SOURCE, which are not visiting files, or which are visiting files
not matching SOURCE, are not modified.

Interactively, TARGET is selected from `tramp-default-rename-alist'
without confirmation if the prefix argument is non-nil.

The remote connection identified by SOURCE is flushed by
`tramp-cleanup-connection'.

\(fn SOURCE TARGET)" t nil)

(autoload 'tramp-rename-these-files "tramp-cmds" "\
Replace visiting file names to TARGET.
The current buffer must be related to a remote connection.  In
all buffers, which are visiting a file with the same directory
name, the buffer file name is changed.

Interactively, TARGET is selected from `tramp-default-rename-alist'
without confirmation if the prefix argument is non-nil.

For details, see `tramp-rename-files'.

\(fn TARGET)" t nil)

(function-put #'tramp-rename-these-files 'completion-predicate #'tramp-command-completion-p)

(autoload 'tramp-recompile-elpa-command-completion-p "tramp-cmds" "\
A predicate for `tramp-recompile-elpa'.
It is completed by \"M-x TAB\" only if package.el is loaded, and
Tramp is an installed ELPA package.

\(fn SYMBOL BUFFER)" nil nil)

(autoload 'tramp-recompile-elpa "tramp-cmds" "\
Recompile the installed Tramp ELPA package.
This is needed if there are compatibility problems." t nil)

(function-put #'tramp-recompile-elpa 'completion-predicate #'tramp-recompile-elpa-command-completion-p)

(autoload 'tramp-version "tramp-cmds" "\
Print version number of tramp.el in echo area or current buffer.

\(fn ARG)" t nil)

(autoload 'tramp-bug "tramp-cmds" "\
Submit a bug report to the Tramp developers." t nil)

;;;***

;;;### (autoloads nil "tramp-compat" "tramp-compat.el" (0 0 0 0))
;;; Generated autoloads from tramp-compat.el

(defalias 'tramp-compat-file-name-quoted-p (if (equal (func-arity #'file-name-quoted-p) '(1 . 2)) #'file-name-quoted-p (lambda (name &optional top) "Whether NAME is quoted with prefix \"/:\".\nIf NAME is a remote file name and TOP is nil, check the local part of NAME." (let ((file-name-handler-alist (unless top file-name-handler-alist))) (string-prefix-p "/:" (file-local-name name))))))

;;;***

;;;### (autoloads nil "tramp-container" "tramp-container.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from tramp-container.el

(defvar tramp-docker-program "docker" "\
Name of the Docker client program.")

(custom-autoload 'tramp-docker-program "tramp-container" t)

(defvar tramp-podman-program "podman" "\
Name of the Podman client program.")

(custom-autoload 'tramp-podman-program "tramp-container" t)

(defvar tramp-kubernetes-program "kubectl" "\
Name of the Kubernetes client program.")

(custom-autoload 'tramp-kubernetes-program "tramp-container" t)

(defconst tramp-docker-method "docker" "\
Tramp method name to use to connect to Docker containers.")

(defconst tramp-podman-method "podman" "\
Tramp method name to use to connect to Podman containers.")

(defconst tramp-kubernetes-method "kubernetes" "\
Tramp method name to use to connect to Kubernetes containers.")

(autoload 'tramp-docker--completion-function "tramp-container" "\
List Docker-like containers available for connection.

This function is used by `tramp-set-completion-function', please
see its function help for a description of the format.

\(fn &rest ARGS)" nil nil)

(autoload 'tramp-kubernetes--completion-function "tramp-container" "\
List Kubernetes pods available for connection.

This function is used by `tramp-set-completion-function', please
see its function help for a description of the format.

\(fn &rest ARGS)" nil nil)

(defvar tramp-default-remote-shell)

(tramp--with-startup (add-to-list 'tramp-methods `(,tramp-docker-method (tramp-login-program ,tramp-docker-program) (tramp-login-args (("exec") ("-it") ("-u" "%u") ("%h") ("%l"))) (tramp-direct-async (,tramp-default-remote-shell "-c")) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-i" "-c")))) (add-to-list 'tramp-methods `(,tramp-podman-method (tramp-login-program ,tramp-podman-program) (tramp-login-args (("exec") ("-it") ("-u" "%u") ("%h") ("%l"))) (tramp-direct-async (,tramp-default-remote-shell "-c")) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-i" "-c")))) (add-to-list 'tramp-methods `(,tramp-kubernetes-method (tramp-login-program ,tramp-kubernetes-program) (tramp-login-args (("exec") ("%h") ("-it") ("--") ("%l"))) (tramp-config-check tramp-kubernetes--current-context-data) (tramp-direct-async (,tramp-default-remote-shell "-c")) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-i" "-c")))) (tramp-set-completion-function tramp-docker-method '((tramp-docker--completion-function ""))) (tramp-set-completion-function tramp-podman-method '((tramp-docker--completion-function ""))) (tramp-set-completion-function tramp-kubernetes-method '((tramp-kubernetes--completion-function ""))))

;;;***

;;;### (autoloads nil "tramp-crypt" "tramp-crypt.el" (0 0 0 0))
;;; Generated autoloads from tramp-crypt.el

(defvar tramp-crypt-enabled nil "\
Non-nil when encryption support is available.")

(defconst tramp-crypt-encfs-config ".encfs6.xml" "\
Encfs configuration file name.")

(defvar tramp-crypt-directories nil "\
List of encrypted remote directories.")

(defsubst tramp-crypt-file-name-p (name) "\
Return the encrypted remote directory NAME belongs to.
If NAME doesn't belong to an encrypted remote directory, return nil." (catch 'crypt-file-name-p (and tramp-crypt-enabled (stringp name) (not (tramp-compat-file-name-quoted-p name)) (not (string-suffix-p tramp-crypt-encfs-config name)) (dolist (dir tramp-crypt-directories) (and (string-prefix-p dir (file-name-as-directory (expand-file-name name))) (throw 'crypt-file-name-p dir))))))

(defconst tramp-crypt-file-name-handler-alist '((access-file . tramp-crypt-handle-access-file) (add-name-to-file . tramp-handle-add-name-to-file) (copy-directory . tramp-handle-copy-directory) (copy-file . tramp-crypt-handle-copy-file) (delete-directory . tramp-crypt-handle-delete-directory) (delete-file . tramp-crypt-handle-delete-file) (directory-files . tramp-crypt-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . ignore) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . tramp-crypt-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-crypt-handle-file-executable-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-handle-file-local-copy) (file-locked-p . tramp-crypt-handle-file-locked-p) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-crypt-handle-file-name-all-completions) (file-name-case-insensitive-p . ignore) (file-name-completion . tramp-handle-file-name-completion) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . tramp-crypt-handle-file-ownership-preserved-p) (file-readable-p . tramp-crypt-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-selinux-context . ignore) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-crypt-handle-file-system-info) (file-writable-p . tramp-crypt-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-crypt-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (list-system-processes . ignore) (load . tramp-handle-load) (lock-file . tramp-crypt-handle-lock-file) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-crypt-handle-make-directory) (make-directory-internal . ignore) (make-lock-file-name . tramp-handle-make-lock-file-name) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . ignore) (make-symbolic-link . tramp-handle-make-symbolic-link) (memory-info . ignore) (process-attributes . ignore) (process-file . ignore) (rename-file . tramp-crypt-handle-rename-file) (set-file-acl . ignore) (set-file-modes . tramp-crypt-handle-set-file-modes) (set-file-selinux-context . ignore) (set-file-times . tramp-crypt-handle-set-file-times) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . ignore) (start-file-process . ignore) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-set-file-uid-gid . tramp-crypt-handle-set-file-uid-gid) (unhandled-file-name-directory . ignore) (unlock-file . tramp-crypt-handle-unlock-file) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-handle-write-region)) "\
Alist of handler functions for crypt method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(autoload 'tramp-crypt-file-name-handler "tramp-crypt" "\
Invoke the encrypted remote file related OPERATION.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(defun tramp-register-crypt-file-name-handler nil "\
Add crypt file name handler to `file-name-handler-alist'." (when (and tramp-crypt-enabled tramp-crypt-directories) (add-to-list 'file-name-handler-alist (cons tramp-file-name-regexp #'tramp-crypt-file-name-handler)) (put #'tramp-crypt-file-name-handler 'safe-magic t)))

(autoload 'tramp-crypt-add-directory "tramp-crypt" "\
Mark remote directory NAME for encryption.
Files in that directory and all subdirectories will be encrypted
before copying to, and decrypted after copying from that
directory.  File names will be also encrypted.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads nil "tramp-ftp" "tramp-ftp.el" (0 0 0 0))
;;; Generated autoloads from tramp-ftp.el

(autoload 'tramp-ftp-enable-ange-ftp "tramp-ftp" "\
Reenable Ange-FTP, when Tramp is unloaded." nil nil)

(defconst tramp-ftp-method "ftp" "\
When this method name is used, forward all calls to Ange-FTP.")

(tramp--with-startup (add-to-list 'tramp-methods (cons tramp-ftp-method nil)) (add-to-list 'tramp-default-method-alist (list (rx bos "ftp.") nil tramp-ftp-method)) (add-to-list 'tramp-default-method-alist (list nil (rx bos (| "anonymous" "ftp") eos) tramp-ftp-method)) (tramp-set-completion-function tramp-ftp-method '((tramp-parse-netrc "~/.netrc"))))

(autoload 'tramp-ftp-file-name-handler "tramp-ftp" "\
Invoke the Ange-FTP handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(defsubst tramp-ftp-file-name-p (vec-or-filename) "\
Check if it's a VEC-OR-FILENAME that should be forwarded to Ange-FTP." (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename))) (string= (tramp-file-name-method vec) tramp-ftp-method)))

(tramp--with-startup (add-to-list 'tramp-foreign-file-name-handler-alist (cons #'tramp-ftp-file-name-p #'tramp-ftp-file-name-handler)))

;;;***

;;;### (autoloads nil "tramp-gvfs" "tramp-gvfs.el" (0 0 0 0))
;;; Generated autoloads from tramp-gvfs.el

(defvar tramp-gvfs-methods '("afp" "dav" "davs" "gdrive" "mtp" "nextcloud" "sftp") "\
List of methods for remote files, accessed with GVFS.")

(custom-autoload 'tramp-gvfs-methods "tramp-gvfs" t)

(defconst tramp-goa-methods '("gdrive" "nextcloud") "\
List of methods which require registration at GNOME Online Accounts.")

(defvar tramp-media-methods '("afc" "gphoto2" "mtp") "\
List of GVFS methods which are covered by the \"mtp\" method.
They are checked during start up via
`tramp-gvfs-interface-remotevolumemonitor'.")

(when (featurep 'dbusbind) (tramp--with-startup (dolist (method tramp-gvfs-methods) (unless (assoc method tramp-methods) (add-to-list 'tramp-methods `(,method))) (when (member method tramp-goa-methods) (add-to-list 'tramp-default-host-alist `(,method nil ""))))))

(defconst tramp-goa-service "org.gnome.OnlineAccounts" "\
The well known name of the GNOME Online Accounts service.")

(defconst tramp-gvfs-service-afc-volumemonitor "org.gtk.vfs.AfcVolumeMonitor" "\
The well known name of the AFC volume monitor.")

(defconst tramp-gvfs-service-gphoto2-volumemonitor "org.gtk.vfs.GPhoto2VolumeMonitor" "\
The well known name of the GPhoto2 volume monitor.")

(defconst tramp-gvfs-service-mtp-volumemonitor "org.gtk.vfs.MTPVolumeMonitor" "\
The well known name of the MTP volume monitor.")

(defconst tramp-gvfs-file-name-handler-alist '((abbreviate-file-name . tramp-handle-abbreviate-file-name) (access-file . tramp-handle-access-file) (add-name-to-file . tramp-handle-add-name-to-file) (copy-directory . tramp-handle-copy-directory) (copy-file . tramp-gvfs-handle-copy-file) (delete-directory . tramp-gvfs-handle-delete-directory) (delete-file . tramp-gvfs-handle-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . ignore) (expand-file-name . tramp-gvfs-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . tramp-gvfs-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-gvfs-handle-file-executable-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-handle-file-local-copy) (file-locked-p . tramp-handle-file-locked-p) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-gvfs-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-gvfs-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-gvfs-handle-file-system-info) (file-truename . tramp-handle-file-truename) (file-writable-p . tramp-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (list-system-processes . ignore) (load . tramp-handle-load) (lock-file . tramp-handle-lock-file) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-gvfs-handle-make-directory) (make-directory-internal . ignore) (make-lock-file-name . tramp-handle-make-lock-file-name) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . ignore) (make-symbolic-link . tramp-handle-make-symbolic-link) (memory-info . ignore) (process-attributes . ignore) (process-file . ignore) (rename-file . tramp-gvfs-handle-rename-file) (set-file-acl . ignore) (set-file-modes . tramp-gvfs-handle-set-file-modes) (set-file-selinux-context . ignore) (set-file-times . tramp-gvfs-handle-set-file-times) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . ignore) (start-file-process . ignore) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-home-directory . tramp-gvfs-handle-get-home-directory) (tramp-get-remote-gid . tramp-gvfs-handle-get-remote-gid) (tramp-get-remote-groups . ignore) (tramp-get-remote-uid . tramp-gvfs-handle-get-remote-uid) (tramp-set-file-uid-gid . tramp-gvfs-handle-set-file-uid-gid) (unhandled-file-name-directory . ignore) (unlock-file . tramp-handle-unlock-file) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-handle-write-region)) "\
Alist of handler functions for Tramp GVFS method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defsubst tramp-gvfs-file-name-p (vec-or-filename) "\
Check if it's a VEC-OR-FILENAME handled by the GVFS daemon." (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename))) (let ((method (tramp-file-name-method vec))) (and (stringp method) (member method tramp-gvfs-methods)))))

(autoload 'tramp-gvfs-file-name-handler "tramp-gvfs" "\
Invoke the GVFS related OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(when (featurep 'dbusbind) (tramp--with-startup (tramp-register-foreign-file-name-handler #'tramp-gvfs-file-name-p #'tramp-gvfs-file-name-handler)))

;;;***

;;;### (autoloads nil "tramp-rclone" "tramp-rclone.el" (0 0 0 0))
;;; Generated autoloads from tramp-rclone.el

(defconst tramp-rclone-method "rclone" "\
When this method name is used, forward all calls to rclone mounts.")

(tramp--with-startup (add-to-list 'tramp-methods `(,tramp-rclone-method (tramp-mount-args ("--no-unicode-normalization" "--dir-cache-time" "0s")) (tramp-copyto-args nil) (tramp-moveto-args nil) (tramp-about-args ("--full")))) (add-to-list 'tramp-default-host-alist `(,tramp-rclone-method nil "")) (tramp-set-completion-function tramp-rclone-method '((tramp-rclone-parse-device-names ""))))

(defconst tramp-rclone-file-name-handler-alist '((access-file . tramp-handle-access-file) (add-name-to-file . tramp-handle-add-name-to-file) (copy-directory . tramp-handle-copy-directory) (copy-file . tramp-rclone-handle-copy-file) (delete-directory . tramp-fuse-handle-delete-directory) (delete-file . tramp-fuse-handle-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-fuse-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . ignore) (expand-file-name . tramp-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . tramp-fuse-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-fuse-handle-file-executable-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-handle-file-local-copy) (file-locked-p . tramp-handle-file-locked-p) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-fuse-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-rclone-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-rclone-handle-file-system-info) (file-truename . tramp-handle-file-truename) (file-writable-p . tramp-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (list-system-processes . ignore) (load . tramp-handle-load) (lock-file . tramp-handle-lock-file) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-fuse-handle-make-directory) (make-directory-internal . ignore) (make-lock-file-name . tramp-handle-make-lock-file-name) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . ignore) (make-symbolic-link . tramp-handle-make-symbolic-link) (memory-info . ignore) (process-attributes . ignore) (process-file . ignore) (rename-file . tramp-rclone-handle-rename-file) (set-file-acl . ignore) (set-file-modes . ignore) (set-file-selinux-context . ignore) (set-file-times . ignore) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . ignore) (start-file-process . ignore) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-home-directory . ignore) (tramp-get-remote-gid . ignore) (tramp-get-remote-groups . ignore) (tramp-get-remote-uid . ignore) (tramp-set-file-uid-gid . ignore) (unhandled-file-name-directory . ignore) (unlock-file . tramp-handle-unlock-file) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-handle-write-region)) "\
Alist of handler functions for Tramp RCLONE method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defsubst tramp-rclone-file-name-p (vec-or-filename) "\
Check if it's a VEC-OR-FILENAME for rclone." (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename))) (string= (tramp-file-name-method vec) tramp-rclone-method)))

(autoload 'tramp-rclone-file-name-handler "tramp-rclone" "\
Invoke the rclone handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(tramp--with-startup (tramp-register-foreign-file-name-handler #'tramp-rclone-file-name-p #'tramp-rclone-file-name-handler))

(autoload 'tramp-rclone-parse-device-names "tramp-rclone" "\
Return a list of (nil host) tuples allowed to access.

\(fn IGNORE)" nil nil)

;;;***

;;;### (autoloads nil "tramp-sh" "tramp-sh.el" (0 0 0 0))
;;; Generated autoloads from tramp-sh.el

(defconst tramp-default-remote-shell "/bin/sh" "\
The default remote shell Tramp applies.")

(defconst tramp-display-escape-sequence-regexp (rx "\33" (+ (any ";[" digit)) "m") "\
Terminal control escape sequences for display attributes.")

(defconst tramp-initial-end-of-output "#$ " "\
Prompt when establishing a connection.")

(tramp--with-startup (add-to-list 'tramp-methods `("rcp" (tramp-login-program "rsh") (tramp-login-args (("%h") ("-l" "%u"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "rcp") (tramp-copy-args (("-p" "%k") ("-r"))) (tramp-copy-keep-date t) (tramp-copy-recursive t))) (add-to-list 'tramp-methods `("remcp" (tramp-login-program "remsh") (tramp-login-args (("%h") ("-l" "%u"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "rcp") (tramp-copy-args (("-p" "%k"))) (tramp-copy-keep-date t))) (add-to-list 'tramp-methods `("scp" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-direct-async t) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "scp") (tramp-copy-args (("-P" "%p") ("-p" "%k") ("%x") ("%y") ("%z") ("-q") ("-r") ("%c"))) (tramp-copy-keep-date t) (tramp-copy-recursive t))) (add-to-list 'tramp-methods `("scpx" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("-t" "-t") ("-o" "RemoteCommand=\"%l\"") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "scp") (tramp-copy-args (("-P" "%p") ("-p" "%k") ("%x") ("%y") ("%z") ("-q") ("-r") ("%c"))) (tramp-copy-keep-date t) (tramp-copy-recursive t))) (add-to-list 'tramp-methods `("rsync" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-direct-async t) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "rsync") (tramp-copy-args (("-t" "%k") ("-p") ("-r") ("-s") ("-c"))) (tramp-copy-env (("RSYNC_RSH") ("ssh") ("%c"))) (tramp-copy-keep-date t) (tramp-copy-keep-tmpfile t) (tramp-copy-recursive t))) (add-to-list 'tramp-methods `("rsh" (tramp-login-program "rsh") (tramp-login-args (("%h") ("-l" "%u"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("remsh" (tramp-login-program "remsh") (tramp-login-args (("%h") ("-l" "%u"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("ssh" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-direct-async t) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("sshx" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("-t" "-t") ("-o" "RemoteCommand=\"%l\"") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("telnet" (tramp-login-program "telnet") (tramp-login-args (("%h") ("%p") ("%n"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("nc" (tramp-login-program "telnet") (tramp-login-args (("%h") ("%p") ("%n"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "nc") (tramp-copy-args (("-w" "1") ("-v") ("%h") ("%r"))) (tramp-remote-copy-program "nc") (tramp-remote-copy-args (("-l") ("-p" "%r") ("%n"))))) (add-to-list 'tramp-methods `("su" (tramp-login-program "su") (tramp-login-args (("-") ("%u"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-connection-timeout 10))) (add-to-list 'tramp-methods `("sg" (tramp-login-program "sg") (tramp-login-args (("-") ("%u"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-args ("-c")) (tramp-connection-timeout 10))) (add-to-list 'tramp-methods `("sudo" (tramp-login-program "env") (tramp-login-args (("SUDO_PROMPT=P\"\"a\"\"s\"\"s\"\"w\"\"o\"\"r\"\"d\"\":") ("sudo") ("-u" "%u") ("-s") ("-H") ("%l"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-connection-timeout 10) (tramp-session-timeout 300) (tramp-password-previous-hop t))) (add-to-list 'tramp-methods `("doas" (tramp-login-program "doas") (tramp-login-args (("-u" "%u") ("-s"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-args ("-c")) (tramp-connection-timeout 10) (tramp-session-timeout 300) (tramp-password-previous-hop t))) (add-to-list 'tramp-methods `("ksu" (tramp-login-program "ksu") (tramp-login-args (("%u") ("-q"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-connection-timeout 10))) (add-to-list 'tramp-methods `("krlogin" (tramp-login-program "krlogin") (tramp-login-args (("%h") ("-l" "%u") ("-x"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("plink" (tramp-login-program "plink") (tramp-login-args (("-l" "%u") ("-P" "%p") ("-ssh") ("-t") ("%h") ("\"") (,(format "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'" tramp-terminal-type tramp-initial-end-of-output)) ("%l") ("\""))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("plinkx" (tramp-login-program "plink") (tramp-login-args (("-load") ("%h") ("-t") ("\"") (,(format "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'" tramp-terminal-type tramp-initial-end-of-output)) ("%l") ("\""))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("pscp" (tramp-login-program "plink") (tramp-login-args (("-l" "%u") ("-P" "%p") ("-ssh") ("-t") ("%h") ("\"") (,(format "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'" tramp-terminal-type tramp-initial-end-of-output)) ("%l") ("\""))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "pscp") (tramp-copy-args (("-l" "%u") ("-P" "%p") ("-scp") ("-p" "%k") ("-q") ("-r"))) (tramp-copy-keep-date t) (tramp-copy-recursive t))) (add-to-list 'tramp-methods `("psftp" (tramp-login-program "plink") (tramp-login-args (("-l" "%u") ("-P" "%p") ("-ssh") ("-t") ("%h") ("\"") (,(format "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'" tramp-terminal-type tramp-initial-end-of-output)) ("%l") ("\""))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "pscp") (tramp-copy-args (("-l" "%u") ("-P" "%p") ("-sftp") ("-p" "%k") ("-q"))) (tramp-copy-keep-date t))) (add-to-list 'tramp-methods `("fcp" (tramp-login-program "fsh") (tramp-login-args (("%h") ("-l" "%u") ("sh" "-i"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-i") ("-c")) (tramp-copy-program "fcp") (tramp-copy-args (("-p" "%k"))) (tramp-copy-keep-date t))) (add-to-list 'tramp-default-method-alist `(,tramp-local-host-regexp ,(tramp-compat-rx bos (literal tramp-root-id-string) eos) "su")) (add-to-list 'tramp-default-user-alist `(,(rx bos (| "su" "sudo" "doas" "ksu") eos) nil ,tramp-root-id-string)) (add-to-list 'tramp-default-user-alist `(,(rx bos (| "rcp" "remcp" "rsh" "telnet" "nc" "krlogin" "fcp") eos) nil ,(user-login-name))))

(defconst tramp-completion-function-alist-rsh '((tramp-parse-rhosts "/etc/hosts.equiv") (tramp-parse-rhosts "~/.rhosts")) "\
Default list of (FUNCTION FILE) pairs to be examined for rsh methods.")

(defconst tramp-completion-function-alist-ssh `((tramp-parse-rhosts "/etc/hosts.equiv") (tramp-parse-rhosts "/etc/shosts.equiv") (tramp-parse-shosts ,(expand-file-name "ssh/ssh_known_hosts" (or (and (eq system-type 'windows-nt) (getenv "ProgramData")) "/etc/"))) (tramp-parse-sconfig ,(expand-file-name "ssh/ssh_config" (or (and (eq system-type 'windows-nt) (getenv "ProgramData")) "/etc/"))) (tramp-parse-shostkeys "/etc/ssh2/hostkeys") (tramp-parse-sknownhosts "/etc/ssh2/knownhosts") (tramp-parse-rhosts "~/.rhosts") (tramp-parse-rhosts "~/.shosts") (tramp-parse-shosts ,(expand-file-name ".ssh/known_hosts" (or (and (eq system-type 'windows-nt) (getenv "USERPROFILE")) "~/"))) (tramp-parse-sconfig ,(expand-file-name ".ssh/config" (or (and (eq system-type 'windows-nt) (getenv "USERPROFILE")) "~/"))) (tramp-parse-shostkeys "~/.ssh2/hostkeys") (tramp-parse-sknownhosts "~/.ssh2/knownhosts")) "\
Default list of (FUNCTION FILE) pairs to be examined for ssh methods.")

(defconst tramp-completion-function-alist-telnet '((tramp-parse-hosts "/etc/hosts")) "\
Default list of (FUNCTION FILE) pairs to be examined for telnet methods.")

(defconst tramp-completion-function-alist-su '((tramp-parse-passwd "/etc/passwd")) "\
Default list of (FUNCTION FILE) pairs to be examined for su methods.")

(defconst tramp-completion-function-alist-sg '((tramp-parse-etc-group "/etc/group")) "\
Default list of (FUNCTION FILE) pairs to be examined for sg methods.")

(defconst tramp-completion-function-alist-putty `((tramp-parse-putty ,(if (eq system-type 'windows-nt) "HKEY_CURRENT_USER\\Software\\SimonTatham\\PuTTY\\Sessions" "~/.putty/sessions"))) "\
Default list of (FUNCTION REGISTRY) pairs to be examined for putty sessions.")

(tramp--with-startup (tramp-set-completion-function "rcp" tramp-completion-function-alist-rsh) (tramp-set-completion-function "remcp" tramp-completion-function-alist-rsh) (tramp-set-completion-function "scp" tramp-completion-function-alist-ssh) (tramp-set-completion-function "scpx" tramp-completion-function-alist-ssh) (tramp-set-completion-function "rsync" tramp-completion-function-alist-ssh) (tramp-set-completion-function "rsh" tramp-completion-function-alist-rsh) (tramp-set-completion-function "remsh" tramp-completion-function-alist-rsh) (tramp-set-completion-function "ssh" tramp-completion-function-alist-ssh) (tramp-set-completion-function "sshx" tramp-completion-function-alist-ssh) (tramp-set-completion-function "telnet" tramp-completion-function-alist-telnet) (tramp-set-completion-function "nc" tramp-completion-function-alist-telnet) (tramp-set-completion-function "su" tramp-completion-function-alist-su) (tramp-set-completion-function "sudo" tramp-completion-function-alist-su) (tramp-set-completion-function "doas" tramp-completion-function-alist-su) (tramp-set-completion-function "ksu" tramp-completion-function-alist-su) (tramp-set-completion-function "sg" tramp-completion-function-alist-sg) (tramp-set-completion-function "krlogin" tramp-completion-function-alist-rsh) (tramp-set-completion-function "plink" tramp-completion-function-alist-ssh) (tramp-set-completion-function "plinkx" tramp-completion-function-alist-putty) (tramp-set-completion-function "pscp" tramp-completion-function-alist-ssh) (tramp-set-completion-function "psftp" tramp-completion-function-alist-ssh) (tramp-set-completion-function "fcp" tramp-completion-function-alist-ssh))

(defconst tramp-sh-file-name-handler-alist '((abbreviate-file-name . tramp-handle-abbreviate-file-name) (access-file . tramp-handle-access-file) (add-name-to-file . tramp-sh-handle-add-name-to-file) (copy-directory . tramp-sh-handle-copy-directory) (copy-file . tramp-sh-handle-copy-file) (delete-directory . tramp-sh-handle-delete-directory) (delete-file . tramp-sh-handle-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-handle-directory-files) (directory-files-and-attributes . tramp-sh-handle-directory-files-and-attributes) (dired-compress-file . tramp-sh-handle-dired-compress-file) (dired-uncache . tramp-handle-dired-uncache) (exec-path . tramp-sh-handle-exec-path) (expand-file-name . tramp-sh-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . tramp-sh-handle-file-acl) (file-attributes . tramp-sh-handle-file-attributes) (file-directory-p . tramp-sh-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-sh-handle-file-executable-p) (file-exists-p . tramp-sh-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-sh-handle-file-local-copy) (file-locked-p . tramp-handle-file-locked-p) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-sh-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-sh-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . tramp-sh-handle-file-ownership-preserved-p) (file-readable-p . tramp-sh-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-sh-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-sh-handle-file-system-info) (file-truename . tramp-sh-handle-file-truename) (file-writable-p . tramp-sh-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-sh-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (list-system-processes . tramp-handle-list-system-processes) (load . tramp-handle-load) (lock-file . tramp-handle-lock-file) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-sh-handle-make-directory) (make-lock-file-name . tramp-handle-make-lock-file-name) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . tramp-sh-handle-make-process) (make-symbolic-link . tramp-sh-handle-make-symbolic-link) (memory-info . tramp-handle-memory-info) (process-attributes . tramp-handle-process-attributes) (process-file . tramp-sh-handle-process-file) (rename-file . tramp-sh-handle-rename-file) (set-file-acl . tramp-sh-handle-set-file-acl) (set-file-modes . tramp-sh-handle-set-file-modes) (set-file-selinux-context . tramp-sh-handle-set-file-selinux-context) (set-file-times . tramp-sh-handle-set-file-times) (set-visited-file-modtime . tramp-sh-handle-set-visited-file-modtime) (shell-command . tramp-handle-shell-command) (start-file-process . tramp-handle-start-file-process) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-home-directory . tramp-sh-handle-get-home-directory) (tramp-get-remote-gid . tramp-sh-handle-get-remote-gid) (tramp-get-remote-groups . tramp-sh-handle-get-remote-groups) (tramp-get-remote-uid . tramp-sh-handle-get-remote-uid) (tramp-set-file-uid-gid . tramp-sh-handle-set-file-uid-gid) (unhandled-file-name-directory . ignore) (unlock-file . tramp-handle-unlock-file) (vc-registered . tramp-sh-handle-vc-registered) (verify-visited-file-modtime . tramp-sh-handle-verify-visited-file-modtime) (write-region . tramp-sh-handle-write-region)) "\
Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

(autoload 'tramp-sh-file-name-handler "tramp-sh" "\
Invoke remote-shell Tramp file name handler.
Fall back to normal file name handler if no Tramp handler exists.

\(fn OPERATION &rest ARGS)" nil nil)

(autoload 'tramp-sh-file-name-handler-p "tramp-sh" "\
Whether VEC uses a method from `tramp-sh-file-name-handler'.

\(fn VEC)" nil nil)

(tramp--with-startup (tramp-register-foreign-file-name-handler #'identity #'tramp-sh-file-name-handler 'append))

;;;***

;;;### (autoloads nil "tramp-smb" "tramp-smb.el" (0 0 0 0))
;;; Generated autoloads from tramp-smb.el

(defconst tramp-smb-method "smb" "\
Method to connect SAMBA and M$ SMB servers.")

(unless (memq system-type '(cygwin windows-nt)) (tramp--with-startup (add-to-list 'tramp-methods `(,tramp-smb-method (tramp-tmpdir "/C$/Temp") (tramp-case-insensitive t)))))

(tramp--with-startup (add-to-list 'tramp-default-user-alist `(,(tramp-compat-rx bos (literal tramp-smb-method) eos) nil nil)) (tramp-set-completion-function tramp-smb-method '((tramp-parse-netrc "~/.netrc"))))

(defconst tramp-smb-file-name-handler-alist '((abbreviate-file-name . tramp-handle-abbreviate-file-name) (access-file . tramp-handle-access-file) (add-name-to-file . tramp-smb-handle-add-name-to-file) (copy-directory . tramp-smb-handle-copy-directory) (copy-file . tramp-smb-handle-copy-file) (delete-directory . tramp-smb-handle-delete-directory) (delete-file . tramp-smb-handle-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . ignore) (expand-file-name . tramp-smb-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . tramp-smb-handle-file-acl) (file-attributes . tramp-smb-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-handle-file-exists-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-smb-handle-file-local-copy) (file-locked-p . tramp-handle-file-locked-p) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-smb-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-handle-file-exists-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-smb-handle-file-system-info) (file-truename . tramp-handle-file-truename) (file-writable-p . tramp-smb-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-smb-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (list-system-processes . ignore) (load . tramp-handle-load) (lock-file . tramp-handle-lock-file) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-smb-handle-make-directory) (make-directory-internal . ignore) (make-lock-file-name . tramp-handle-make-lock-file-name) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . ignore) (make-symbolic-link . tramp-smb-handle-make-symbolic-link) (memory-info . ignore) (process-attributes . ignore) (process-file . tramp-smb-handle-process-file) (rename-file . tramp-smb-handle-rename-file) (set-file-acl . tramp-smb-handle-set-file-acl) (set-file-modes . tramp-smb-handle-set-file-modes) (set-file-selinux-context . ignore) (set-file-times . ignore) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . tramp-handle-shell-command) (start-file-process . tramp-smb-handle-start-file-process) (substitute-in-file-name . tramp-smb-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-home-directory . tramp-smb-handle-get-home-directory) (tramp-get-remote-gid . ignore) (tramp-get-remote-groups . ignore) (tramp-get-remote-uid . ignore) (tramp-set-file-uid-gid . ignore) (unhandled-file-name-directory . ignore) (unlock-file . tramp-handle-unlock-file) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-smb-handle-write-region)) "\
Alist of handler functions for Tramp SMB method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defsubst tramp-smb-file-name-p (vec-or-filename) "\
Check if it's a VEC-OR-FILENAME for SMB servers." (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename))) (string= (tramp-file-name-method vec) tramp-smb-method)))

(autoload 'tramp-smb-file-name-handler "tramp-smb" "\
Invoke the SMB related OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(unless (memq system-type '(cygwin windows-nt)) (tramp--with-startup (tramp-register-foreign-file-name-handler #'tramp-smb-file-name-p #'tramp-smb-file-name-handler)))

;;;***

;;;### (autoloads nil "tramp-sshfs" "tramp-sshfs.el" (0 0 0 0))
;;; Generated autoloads from tramp-sshfs.el

(defconst tramp-sshfs-method "sshfs" "\
Tramp method for sshfs mounts.")

(defvar tramp-default-remote-shell)

(tramp--with-startup (add-to-list 'tramp-methods `(,tramp-sshfs-method (tramp-mount-args (("-C") ("-p" "%p") ("-o" "dir_cache=no") ("-o" "transform_symlinks") ("-o" "idmap=user,reconnect"))) (tramp-login-program "ssh") (tramp-login-args (("-q") ("-l" "%u") ("-p" "%p") ("-e" "none") ("-t" "-t") ("%h") ("%l"))) (tramp-direct-async t) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-connection-properties `(,(format "/%s:" tramp-sshfs-method) "direct-async-process" t)) (tramp-set-completion-function tramp-sshfs-method tramp-completion-function-alist-ssh))

(defconst tramp-sshfs-file-name-handler-alist '((access-file . tramp-handle-access-file) (add-name-to-file . tramp-handle-add-name-to-file) (copy-directory . tramp-handle-copy-directory) (copy-file . tramp-sshfs-handle-copy-file) (delete-directory . tramp-fuse-handle-delete-directory) (delete-file . tramp-fuse-handle-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-fuse-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . tramp-sshfs-handle-exec-path) (expand-file-name . tramp-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . tramp-fuse-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-fuse-handle-file-executable-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-handle-file-local-copy) (file-locked-p . tramp-handle-file-locked-p) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-fuse-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-sshfs-handle-file-system-info) (file-truename . tramp-handle-file-truename) (file-writable-p . tramp-sshfs-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-handle-insert-directory) (insert-file-contents . tramp-sshfs-handle-insert-file-contents) (list-system-processes . tramp-handle-list-system-processes) (load . tramp-handle-load) (lock-file . tramp-handle-lock-file) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-fuse-handle-make-directory) (make-directory-internal . ignore) (make-lock-file-name . tramp-handle-make-lock-file-name) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . tramp-handle-make-process) (make-symbolic-link . tramp-handle-make-symbolic-link) (memory-info . tramp-handle-memory-info) (process-attributes . tramp-handle-process-attributes) (process-file . tramp-sshfs-handle-process-file) (rename-file . tramp-sshfs-handle-rename-file) (set-file-acl . ignore) (set-file-modes . tramp-sshfs-handle-set-file-modes) (set-file-selinux-context . ignore) (set-file-times . tramp-sshfs-handle-set-file-times) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . tramp-handle-shell-command) (start-file-process . tramp-handle-start-file-process) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-home-directory . ignore) (tramp-get-remote-gid . ignore) (tramp-get-remote-groups . ignore) (tramp-get-remote-uid . ignore) (tramp-set-file-uid-gid . ignore) (unhandled-file-name-directory . ignore) (unlock-file . tramp-handle-unlock-file) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-sshfs-handle-write-region)) "\
Alist of handler functions for Tramp SSHFS method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defsubst tramp-sshfs-file-name-p (vec-or-filename) "\
Check if it's a VEC-OR-FILENAME for sshfs." (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename))) (string= (tramp-file-name-method vec) tramp-sshfs-method)))

(autoload 'tramp-sshfs-file-name-handler "tramp-sshfs" "\
Invoke the sshfs handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(tramp--with-startup (tramp-register-foreign-file-name-handler #'tramp-sshfs-file-name-p #'tramp-sshfs-file-name-handler))

;;;***

;;;### (autoloads nil "tramp-sudoedit" "tramp-sudoedit.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from tramp-sudoedit.el

(defconst tramp-sudoedit-method "sudoedit" "\
When this method name is used, call sudoedit for editing a file.")

(tramp--with-startup (add-to-list 'tramp-methods `(,tramp-sudoedit-method (tramp-sudo-login (("sudo") ("-u" "%u") ("-S") ("-H") ("-p" "Password:") ("--"))) (tramp-password-previous-hop t))) (add-to-list 'tramp-default-user-alist `(,(tramp-compat-rx bos (literal tramp-sudoedit-method) eos) nil ,tramp-root-id-string)) (tramp-set-completion-function tramp-sudoedit-method tramp-completion-function-alist-su))

(defconst tramp-sudoedit-file-name-handler-alist '((abbreviate-file-name . tramp-handle-abbreviate-file-name) (access-file . tramp-handle-access-file) (add-name-to-file . tramp-sudoedit-handle-add-name-to-file) (byte-compiler-base-file-name . ignore) (copy-directory . tramp-handle-copy-directory) (copy-file . tramp-sudoedit-handle-copy-file) (delete-directory . tramp-sudoedit-handle-delete-directory) (delete-file . tramp-sudoedit-handle-delete-file) (diff-latest-backup-file . ignore) (directory-files . tramp-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . ignore) (expand-file-name . tramp-sudoedit-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . tramp-sudoedit-handle-file-acl) (file-attributes . tramp-sudoedit-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-sudoedit-handle-file-executable-p) (file-exists-p . tramp-sudoedit-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-handle-file-local-copy) (file-locked-p . tramp-handle-file-locked-p) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-sudoedit-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-sudoedit-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-sudoedit-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-sudoedit-handle-file-system-info) (file-truename . tramp-sudoedit-handle-file-truename) (file-writable-p . tramp-sudoedit-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (list-system-processes . ignore) (load . tramp-handle-load) (lock-file . tramp-handle-lock-file) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-sudoedit-handle-make-directory) (make-directory-internal . ignore) (make-lock-file-name . tramp-handle-make-lock-file-name) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . ignore) (make-symbolic-link . tramp-sudoedit-handle-make-symbolic-link) (memory-info . ignore) (process-attributes . ignore) (process-file . ignore) (rename-file . tramp-sudoedit-handle-rename-file) (set-file-acl . tramp-sudoedit-handle-set-file-acl) (set-file-modes . tramp-sudoedit-handle-set-file-modes) (set-file-selinux-context . tramp-sudoedit-handle-set-file-selinux-context) (set-file-times . tramp-sudoedit-handle-set-file-times) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . ignore) (start-file-process . ignore) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-home-directory . tramp-sudoedit-handle-get-home-directory) (tramp-get-remote-gid . tramp-sudoedit-handle-get-remote-gid) (tramp-get-remote-groups . tramp-sudoedit-handle-get-remote-groups) (tramp-get-remote-uid . tramp-sudoedit-handle-get-remote-uid) (tramp-set-file-uid-gid . tramp-sudoedit-handle-set-file-uid-gid) (unhandled-file-name-directory . ignore) (unlock-file . tramp-handle-unlock-file) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-handle-write-region)) "\
Alist of handler functions for Tramp SUDOEDIT method.")

(defsubst tramp-sudoedit-file-name-p (vec-or-filename) "\
Check if it's a VEC-OR-FILENAME for SUDOEDIT." (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename))) (string= (tramp-file-name-method vec) tramp-sudoedit-method)))

(autoload 'tramp-sudoedit-file-name-handler "tramp-sudoedit" "\
Invoke the SUDOEDIT handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(tramp--with-startup (tramp-register-foreign-file-name-handler #'tramp-sudoedit-file-name-p #'tramp-sudoedit-file-name-handler))

;;;***

;;;### (autoloads nil "tramp-uu" "tramp-uu.el" (0 0 0 0))
;;; Generated autoloads from tramp-uu.el

(autoload 'tramp-uuencode-region "tramp-uu" "\
UU-encode the region between BEG and END.

\(fn BEG END)" nil nil)

;;;***

;;;### (autoloads nil "trampver" "trampver.el" (0 0 0 0))
;;; Generated autoloads from trampver.el

(defconst tramp-version "2.6.0" "\
This version of Tramp.")

(defconst tramp-bug-report-address "tramp-devel@gnu.org" "\
Email address to send bug reports to.")

;;;***

;;;### (autoloads nil nil ("tramp-fuse.el" "tramp-integration.el")
;;;;;;  (0 0 0 0))

;;;***

(provide 'tramp-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tramp-loaddefs.el ends here
