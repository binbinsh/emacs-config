# Agent Collaboration Rules

## Keybinding Policy (Mandatory)

1. Custom keybindings must prefer single-level `C-c <key>` style.
   - Do not introduce `C-c C-<key>` style bindings unless explicitly requested by the user.

2. Mode-local keybindings must not override existing global keybindings.
   - If a local binding conflicts with a global binding, either remove the local binding or remap it to a non-conflicting key.

3. Any keybinding change must keep documentation in sync.
   - Update the `README.md` keybinding section in the same change.
   - Verify every documented key exists in actual bindings (`init.el` / `post-init.el`).
