# My Haskell & VS Code setup

## Contents:
- Installation/configuration guide for Haskell and VS Code (below)
- Sample VS Code `settings.json` file for my complete setup
- `.stylish-haskell.yaml` file containing stylish-haskell settings for Kowainik style guide
- Sample `FizzBuzz.hs` file for testing GHCI and previewing theme configuration


## 1. Install GHCup:

- Open a new terminal window and enter the following command:

  `~$ curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`

- Press ENTER to proceed
- Press P or A and ENTER to prepend/append PATH variable to your `.bashrc` file
- Press Y and ENTER to install haskell-language-server
- Press Y and ENTER to install stack

- You might now see a list of system requirements. These dependencies must be installed prior to continuing installation.
    - (Linux) Install GHCup dependencies in separate terminal window:

      `~$ sudo apt-get install -y build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`

- **Note:** there is a helpful 'nuke' command if you face any issues with your ghcup installation and need to start over (this may be necessary if you had any previous Haskell installation on your system and encounter errors when using stack)
  
  `~$ ghcup nuke`


## 2. Install hlint (Haskell linter):

- Open a new terminal window and enter the following command:

  `~$ stack install hlint`

  (This will take a while, especially if you've never used Stack on your system)

- **Note:** if you encounter the following warning...

  `Warning: Installation path /home/{username}/.local/bin not found on the PATH environment variable.`

  ...add the directory to PATH with this command:
  
  `~$ export PATH="$HOME/.local/bin:$PATH"`


## 3. Install VS Code & configure for Haskell

- Download and install VS Code (https://code.visualstudio.com/Download)

- Configure `Insert Spaces` to replace tabs with spaces
  - In VS Code, go to `File > Preferences > Settings`
  - Find the `Editor: Insert Spaces` setting
  - Make sure the box is checked next to `Insert spaces when pressing Tab.`

- Install required Haskell extensions:
  - Click the Extensions icon in left panel (looks like four squares with one removed)
  - Search for and install the following extensions
    * `Haskell`
    * `Haskell Syntax Highlighting`
    * `haskell-linter`

## 4. Install stylish-haskell formatter (*recommended*) 
There are several Haskell formatters available that auto-format .hs files. I recommend stylish-haskell because it is popular and easy to install and integrate with VS Code.

- Install stylish-haskell using stack in a terminal window:
  
  `~$ stack install stylish-haskell`
  
  (again, this will take a while)

- Go to Extensions in VS Code and search for/install `stylish-haskell`
- Type `Ctrl + Shift + p` (Linux) or `Shift + Cmd + p` (Mac) to open command palette
- Type "open settings" and select `Open Settings (JSON)`
- In the `settings.json` file, add the following snippet to the bottom
  (make sure that this is enclosed within the outer curly brace, and that the previous entry ends with a comma):

        "[haskell]": {
            "editor.defaultFormatter": "vigoo.stylish-haskell",
            "editor.formatOnSave": true,
        }
     (Omit the second line if you do not want auto-format on save, and follow manual instructions below to format)

- (*Optional*) add the included `.stylish-haskell.yaml` file to your home directory:
    This contains preset configuration for stylish-haskell following Kowainik's
    style guide 
    
    (*Source:* https://github.com/kowainik/org/blob/main/.stylish-haskell.yaml)

- **To manually run stylish-haskell format on your .hs files:**
  - Go to `File > Preferences > Keyboard Shortcuts`
  - Type `Format Document` to find the corresponding keyboard shortcut
    (the default on my Linux setup is `Ctrl + Shift + i`)
  - Use this shortcut anytime to apply the stylish-haskell formatting to your .hs file


## 5. Extra Swag for VS Code
- ### Install Fira Code font to add font ligatures
  - Font ligatures conjoin certain combinations of characters to improve readability/code appearance. Fira Code is a popular open source font that includes ligatures
  - Install the font by following the instructions for your OS: https://github.com/tonsky/FiraCode/wiki/Installing
  - Open the command pallette (`Ctrl + Shift + p` on Linux, `Shift + Cmd + p` on Mac)
  - Search for `Preferences: Open Settings (JSON)` and open it
  - Add the following lines to the `settings.json` file:
      "editor.fontFamily": "'Fira Code'",
      "editor.fontLigatures": true
  - In Extensions, search for/install 'Disable Ligatures'
          (This will disable the ligatures when the cursor is on them)
  - Restart VSCode

- ### Add a vertical ruler at a desired character length
  - A vertical ruler is a helpful visual guide to keep your code lines within a desired length
  - Open the command pallette (`Ctrl + Shift + p` on Linux, `Shift + Cmd + p` on Mac)
  - Search for `Preferences: Open Settings (JSON)` and open it.
  - Add the following lines to the `settings.json` file:

                "editor.rulers": [
                    {
                        "column": 100,
                        "color": "#ff9900"
                    }
                ],

    (This creates an orange ruler at 100 characters. Change the values to suit your preference)

- ### Install vscode-icons extension
  - This extension adds file/language-specific icons to the VS Code Explorer panel
  - There are multiple extensions that style VS Code icons, but this one is the best in my opinion and has the correctly colored purple Haskell icon for .hs files
  - Just search for and install `vscode-icons` in Extensions and icons will automatically be applied

- ### My preferred VS Code Theme: Dracula
  - The VS Code theme I use is called Dracula, and is originally from the Atom code editor
  - Dracula is a dark theme with good color contrast, reducing eyestrain. The color palette also works well with the Haskell syntax highlighting in VS Code
  - Search `Dracula Official` in Extensions to install
  - Click `Set color theme` on the Extension page to set the theme

### See the included `settings.json` file containing complete VS Code settings for the above setup.