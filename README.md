# OutlineNum: Automatically number sections for RMarkdown headers/outline in RStudio

**OutlineNum** is an RStudio addin for adding and removing numbering from headers/outline in your R Markdown in RStudio.

## Features

- Easily add or remove numbering from headers.
- Supports multiple levels of headers.
- Works with R Markdown and plain text documents.

## Installation

OutlineNum is currently available for installation via GitHub. You can install it using the following command:

```R
remotes::install_github("callAgene/OutlineNum")
```

## Usage
To use the OutlineNum addin, please follow these steps:

- Open RStudio and load your document.

- <u>**SELECT ALL**</u> the text in your document by pressing Ctrl+A (Windows/Linux) or Command+A (macOS).

- Navigate to the "Addins" menu in RStudio.
<img src="./vignettes/1.png" width="50%" height="50%">

- Choose either "Add numbering" or "Remove numbering" from the list of available addins.(**NOTE** : The header must start with a letter, if it starts with Chinese or something else it will be deleted.)
<img src="./vignettes/2.png" width="80%" height="80%">

The document titles will be numbered or de-numbered according to your selection.



