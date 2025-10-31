#!/usr/bin/env bash

set -euo pipefail

info() { printf "\033[1;34m[i]\033[0m %s\n" "$*"; }
warn() { printf "\033[1;33m[!]\033[0m %s\n" "$*"; }
err() { printf "\033[1;31m[✘]\033[0m %s\n" "$*"; }
have() { command -v "$1" >/dev/null 2>&1; }

MAIL_BASE="${MAIL_BASE:-$HOME/mail}"

usage() {
  cat <<EOF
Usage: $0 <account1@gmail.com> [account2@gmail.com ...]

Sets up Gmail sync with lieer (gmi) for one or more accounts under:
  $MAIL_BASE/<account>

Notes:
  - This script prefers 'uv run gmi' per your workflow; falls back to 'gmi' if needed.
  - On first sync, a browser OAuth flow will open.
  - 'notmuch' database path will be set to $MAIL_BASE.

Examples:
  $0 [email protected]
  MAIL_BASE="$HOME/.mail" $0 [email protected] [email protected]
EOF
}

choose_gmi_cmd() {
  if have uv && uv run gmi --version >/dev/null 2>&1; then
    echo "uv run gmi"
    return 0
  fi
  if have gmi; then
    echo "gmi"
    return 0
  fi
  err "Neither 'uv run gmi' nor 'gmi' found. Run install-emacs.sh first to install lieer."
  exit 1
}

init_account() {
  local account="$1"
  local acc_dir="$MAIL_BASE/$account"
  mkdir -p "$acc_dir"

  if [ -f "$acc_dir/.gmailieer.json" ]; then
    info "Account already initialized: $account ($acc_dir)"
  else
    info "Initializing lieer for $account at $acc_dir"
    ${GMI_CMD} init -C "$acc_dir" "$account"
  fi
}

sync_account() {
  local account="$1"
  local acc_dir="$MAIL_BASE/$account"
  info "Syncing $account"
  ${GMI_CMD} sync -C "$acc_dir"
}

ensure_notmuch_path() {
  if have notmuch; then
    info "Setting notmuch database.path to $MAIL_BASE"
    notmuch config set database.path "$MAIL_BASE" || true
  else
    warn "notmuch not found; skipping database.path setup"
  fi
}

ensure_notmuch_db() {
  if have notmuch; then
    if [ ! -d "$MAIL_BASE/.notmuch" ]; then
      info "Initializing notmuch database at $MAIL_BASE (creating .notmuch)"
      notmuch new >/dev/null 2>&1 || true
    fi
  fi
}

install_sendmail_wrapper() {
  local bin_dir="$HOME/.local/bin"
  local wrapper="$bin_dir/sendmail-gmi"
  mkdir -p "$bin_dir"
  if [ -x "$wrapper" ]; then
    info "sendmail-gmi already installed at $wrapper"
    return 0
  fi
  info "Installing sendmail-gmi wrapper at $wrapper"
  cat >"$wrapper" <<'WEOF'
#!/usr/bin/env bash
set -euo pipefail

MAIL_BASE="${MAIL_BASE:-$HOME/mail}"

have() { command -v "$1" >/dev/null 2>&1; }

choose_gmi_cmd() {
  if have uv && uv run gmi --version >/dev/null 2>&1; then
    echo "uv run gmi"
    return 0
  fi
  if have gmi; then
    echo "gmi"
    return 0
  fi
  echo ""
}

tmpmsg="$(mktemp)"
trap 'rm -f "$tmpmsg"' EXIT
cat >"$tmpmsg"

# Extract From header email (last occurrence wins if multiple)
from_line="$(grep -i "^From:" "$tmpmsg" | tail -n1 || true)"
if [ -z "$from_line" ]; then
  printf "\033[1;31m[✘]\033[0m %s\n" "No From header found" >&2
  exit 2
fi
from_email="$(printf "%s" "$from_line" | sed -E 's/.*<([^>]+)>.*/\1/;t; s/^From:[[:space:]]*//; s/[[:space:]].*//')"
if [ -z "$from_email" ]; then
  printf "\033[1;31m[✘]\033[0m %s\n" "Could not parse sender email from: $from_line" >&2
  exit 2
fi

# Determine account directory
acc_dir=""
if [ -d "$MAIL_BASE/$from_email" ]; then
  acc_dir="$MAIL_BASE/$from_email"
else
  # Optional mapping file: ~/.config/sendmail-gmi/map with lines: <email> <absolute-acc-dir>
  mapfile="$HOME/.config/sendmail-gmi/map"
  if [ -f "$mapfile" ]; then
    acc_dir="$(awk -v key="$from_email" '$1==key {print $2; found=1} END{ if(!found) exit 1 }' "$mapfile" 2>/dev/null || true)"
  fi
  if [ -z "$acc_dir" ]; then
    # Fallback: if exactly one directory exists under MAIL_BASE, use it
    mapfile -t candidates < <(find "$MAIL_BASE" -mindepth 1 -maxdepth 1 -type d -print)
    if [ "${#candidates[@]}" -eq 1 ]; then
      acc_dir="${candidates[0]}"
    fi
  fi
fi

if [ -z "$acc_dir" ]; then
  printf "\033[1;31m[✘]\033[0m %s\n" "Account directory not found for $from_email under $MAIL_BASE. Create $MAIL_BASE/$from_email or define map in ~/.config/sendmail-gmi/map" >&2
  exit 3
fi

gmi_cmd="$(choose_gmi_cmd)"
if [ -z "$gmi_cmd" ]; then
  printf "\033[1;31m[✘]\033[0m %s\n" "No gmi available. Install lieer (gmi) first." >&2
  exit 4
fi

exec bash -c "$gmi_cmd send -C \"$acc_dir\" < \"$tmpmsg\""
WEOF
  chmod +x "$wrapper"
  info "sendmail-gmi installed. Ensure $bin_dir is in PATH."
}

main() {
  if [ "$#" -lt 1 ]; then
    usage
    exit 1
  fi

  mkdir -p "$MAIL_BASE"
  GMI_CMD="$(choose_gmi_cmd)"

  # Ensure notmuch points at MAIL_BASE and database exists before lieer init
  ensure_notmuch_path
  ensure_notmuch_db

  for acct in "$@"; do
    init_account "$acct"
    sync_account "$acct"
  done

  # Index newly synced mail
  if have notmuch; then
    info "Indexing mail with notmuch new"
    notmuch new || true
  fi
  install_sendmail_wrapper

  info "Done. Open Emacs and run: M-x notmuch"
}

main "$@"


