# ReNGBis Language Server Protocol (LSP)

This module provides Language Server Protocol support for ReNGBis schema files, enabling rich IDE features in any LSP-compatible editor.

## Features

- **Syntax Validation**: Real-time error checking as you type
- **Code Completion**: Auto-complete for keywords, types, and schema syntax
- **Hover Information**: Documentation for schema elements and keywords
- **Go to Definition**: Navigate to named value definitions and imports
- **Error Diagnostics**: Detailed error messages with line/column information

## Building

Build the LSP server JAR:

```bash
sbt lsp/assembly
```

This creates `modules/lsp/target/scala-3.7.4/rengbis-lsp.jar`.

## Running

Use the provided launcher script from the project root:

```bash
./rengbis-lsp
```

Or run the JAR directly:

```bash
java -jar modules/lsp/target/scala-3.7.4/rengbis-lsp.jar
```

The LSP server communicates via stdin/stdout using the JSON-RPC protocol.

## Editor Setup

### VS Code

1. Create a VS Code extension or use a generic LSP client extension like [vscode-lsp-client](https://marketplace.visualstudio.com/items?itemName=matangover.vscode-lsp-client)

2. Add to your workspace settings (`.vscode/settings.json`):

```json
{
  "lsp-client.languageId": "rengbis",
  "lsp-client.serverCommand": "/absolute/path/to/rengbis/rengbis-lsp"
}
```

3. Configure file associations for `.rng` or `.rengbis` files:

```json
{
  "files.associations": {
    "*.rng": "rengbis",
    "*.rengbis": "rengbis"
  }
}
```

#### Creating a Custom VS Code Extension

For a better experience, create a minimal VS Code extension:

1. Create a directory structure:
```
rengbis-vscode/
├── package.json
├── src/
│   └── extension.ts
└── tsconfig.json
```

2. `package.json`:
```json
{
  "name": "rengbis-lsp-client",
  "version": "0.1.0",
  "publisher": "your-name",
  "engines": { "vscode": "^1.75.0" },
  "activationEvents": ["onLanguage:rengbis"],
  "main": "./out/extension.js",
  "contributes": {
    "languages": [{
      "id": "rengbis",
      "extensions": [".rng", ".rengbis"],
      "configuration": "./language-configuration.json"
    }]
  },
  "dependencies": {
    "vscode-languageclient": "^9.0.0"
  },
  "devDependencies": {
    "@types/vscode": "^1.75.0",
    "typescript": "^5.0.0"
  }
}
```

3. `src/extension.ts`:
```typescript
import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const serverCommand = workspace.getConfiguration('rengbis').get<string>('lspPath') 
    || '/absolute/path/to/rengbis-lsp';
  
  const serverOptions: ServerOptions = {
    command: serverCommand,
    args: []
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'rengbis' }]
  };

  client = new LanguageClient(
    'rengbis-lsp',
    'ReNGBis Language Server',
    serverOptions,
    clientOptions
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
```

### Neovim

Using [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig):

```lua
-- In your neovim config (e.g., ~/.config/nvim/init.lua)

local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Define custom LSP configuration
if not configs.rengbis then
  configs.rengbis = {
    default_config = {
      cmd = { '/absolute/path/to/rengbis-lsp' },
      filetypes = { 'rengbis' },
      root_dir = lspconfig.util.root_pattern('.git', 'schemas'),
      settings = {},
    },
  }
end

-- Start the LSP server for rengbis files
lspconfig.rengbis.setup{}

-- Set filetype for .rng and .rengbis files
vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = {"*.rng", "*.rengbis"},
  command = "set filetype=rengbis",
})
```

### Emacs

Using [lsp-mode](https://emacs-lsp.github.io/lsp-mode/):

```elisp
;; Add to your Emacs config (e.g., ~/.emacs.d/init.el)

(require 'lsp-mode)

;; Define rengbis-mode
(define-derived-mode rengbis-mode prog-mode "ReNGBis"
  "Major mode for editing ReNGBis schema files.")

;; Associate file extensions
(add-to-list 'auto-mode-alist '("\\.rng\\'" . rengbis-mode))
(add-to-list 'auto-mode-alist '("\\.rengbis\\'" . rengbis-mode))

;; Register LSP server
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection "/absolute/path/to/rengbis-lsp")
  :major-modes '(rengbis-mode)
  :server-id 'rengbis-lsp))

;; Enable LSP for rengbis-mode
(add-hook 'rengbis-mode-hook #'lsp)
```

### Sublime Text

Using [LSP package](https://packagecontrol.io/packages/LSP):

1. Install the LSP package from Package Control
2. Add to your LSP settings (`Preferences > Package Settings > LSP > Settings`):

```json
{
  "clients": {
    "rengbis": {
      "enabled": true,
      "command": ["/absolute/path/to/rengbis-lsp"],
      "selector": "source.rengbis"
    }
  }
}
```

3. Create a syntax definition file for ReNGBis (save as `ReNGBis.sublime-syntax`):

```yaml
%YAML 1.2
---
name: ReNGBis
file_extensions: [rng, rengbis]
scope: source.rengbis

contexts:
  main:
    - match: '#.*$'
      scope: comment.line.rengbis
    - match: '\b(text|number|boolean|any)\b'
      scope: storage.type.rengbis
    - match: '\b(define|import|as)\b'
      scope: keyword.control.rengbis
```

### IntelliJ IDEA / Other JetBrains IDEs

Use the [LSP Support plugin](https://plugins.jetbrains.com/plugin/10209-lsp-support):

1. Install the LSP Support plugin
2. Go to Settings > Languages & Frameworks > Language Server Protocol > Server Definitions
3. Add a new server definition:
   - Extension: `rng,rengbis`
   - Command: `/absolute/path/to/rengbis-lsp`

### Zed

[Zed](https://zed.dev/) has built-in LSP support. Configure it by editing your Zed settings:

1. Open Zed settings: `Cmd+,` (macOS) or `Ctrl+,` (Linux)
2. Add the following configuration to your `settings.json`:

```json
{
  "languages": {
    "ReNGBis": {
      "language_servers": ["rengbis-lsp"],
      "file_types": ["rng", "rengbis"],
      "line_comments": ["# "]
    }
  },
  "lsp": {
    "rengbis-lsp": {
      "binary": {
        "path": "/absolute/path/to/rengbis-lsp",
        "arguments": []
      }
    }
  }
}
```

3. Restart Zed or reload the window

**Note**: Replace `/absolute/path/to/rengbis-lsp` with the actual absolute path to your launcher script.

#### Alternative: Project-specific configuration

For project-specific settings, create a `.zed/settings.json` file in your project root:

```json
{
  "lsp": {
    "rengbis-lsp": {
      "binary": {
        "path": "/absolute/path/to/rengbis-lsp"
      }
    }
  }
}
```

Then ensure the language configuration is in your global settings as shown above.

#### Creating a Zed Extension (Advanced)

For a more integrated experience, you can create a Zed extension. Create a directory structure:

```
~/.config/zed/extensions/rengbis/
├── extension.toml
└── languages/
    └── rengbis/
        ├── config.toml
        └── highlights.scm (optional syntax highlighting)
```

**extension.toml**:
```toml
id = "rengbis"
name = "ReNGBis"
version = "0.1.0"
authors = ["Your Name <your.email@example.com>"]
description = "ReNGBis schema language support"
```

**languages/rengbis/config.toml**:
```toml
name = "ReNGBis"
file_types = ["rng", "rengbis"]
line_comment = "# "

[language_server]
name = "rengbis-lsp"
language_id = "rengbis"

[language_server.binary]
path = "/absolute/path/to/rengbis-lsp"
args = []
```

After creating the extension, restart Zed to load it.

## Testing the LSP Server

Create a test schema file (`test.rng`):

```rengbis
# This is a comment
= {
    name: text,
    age: number { value >= 0, integer },
    email: text { regex = "^[a-zA-Z0-9+.-]+@[a-zA-Z0-9.-]+$" }
}
```

Open it in your configured editor. You should see:
- Syntax highlighting (if configured)
- Code completion when typing
- Hover information on keywords
- Error messages if you introduce syntax errors

Try hovering over keywords like `text`, `number`, `regex`, `value` to see documentation.

## Troubleshooting

### LSP server not starting

1. Verify the JAR was built: `ls -l modules/lsp/target/scala-3.7.4/rengbis-lsp.jar`
2. Test the launcher script: `./modules/lsp/rengbis-lsp` (it should wait for input)
3. Check your editor's LSP client logs for error messages

### No completions or hover information

1. Ensure the file is recognized as a ReNGBis file (check the language mode in your editor)
2. Check that the LSP server is running (look for the process in your task manager)
3. Enable LSP debug logging in your editor to see communication issues

### Syntax errors not showing

1. The LSP server depends on the core parser from the `core` module
2. Ensure both modules were compiled: `sbt core/compile lsp/compile`
3. Rebuild the JAR: `sbt lsp/assembly`

## Architecture

The LSP module consists of (located in `modules/lsp/src/main/scala/rengbis/lsp/`):

- **Main.scala**: Entry point that launches the LSP server
- **RengbisLanguageServer.scala**: Main LSP server implementation
- **RengbisTextDocumentService.scala**: Handles document-specific operations
- **RengbisWorkspaceService.scala**: Handles workspace-level operations
- **DocumentManager.scala**: Tracks open documents and their state
- **DiagnosticsProvider.scala**: Converts parser errors to LSP diagnostics
- **CompletionProvider.scala**: Provides code completion suggestions
- **HoverProvider.scala**: Provides hover documentation
- **DefinitionProvider.scala**: Provides go-to-definition support

The LSP server reuses the parser and validator from the `core` module, ensuring consistency between the CLI tool and the LSP features.

## Future Enhancements

Potential features to add:
- Document symbols (outline view)
- Semantic highlighting
- Code formatting
- Quick fixes for common errors
- Rename refactoring
- Find references
- Signature help for function-like constructs
- Workspace-wide import resolution
