# About
epub.el is a fast and light-weight Major mode for reading EPUB files within Emacs.

It's inspired by previous projects like nov.el, but rewrites many of the internal xml parsing and query utilities to optimize performance for large EPUB files, especially TOC (table of content) rendering.

epub.el is conformant to the [EPUB 3.3 W3C specification](https://www.w3.org/TR/epub/). It should work for EPUB 2.x files too, though full backward-compatibility is not guaranteed.

epub.el relies (solely) on native package shr.el for xml rendering, CSS and JS scripts are not supported.

# Installation
## Manual
Clone the git repository locally, add the following in your `init.el` file:
```
(add-to-list 'load-path "path/to/local/git/directory/")
;; so that emacs knows where to look for epub.el
(autoload #'epub-mode "epub" nil t)
;; lazy-load package only when opening epub files
```

## straight.el
`straight.el` supports easy installation of packages "straight" from the git repository, add the following to your `init.el` file:
```
(straight-use-package '(epub-mode :type git :host github :repo "peteleng/epub-mode"))
```

## MELPA
Currently unavailable, coming soon.
  
# Features and hotkeys
## View and Navigate EPUB files
| Navigation                     |                         |
|:-------------------------------|:------------------------|
| Scroll up / down page-wise     | space / backspace       |
| Scroll up / down line-wise     | j, C-n / k, C-p         |
| Follow link                    | enter, mouse left-click |
| Beginning / End of page        | <, M-< / >, M->         |
| Next / Previous page           | n / p                   |
| Back / Forward in page history | l / r                   |
| Go to TOC                      | t                       |
| Go to Imenu chapter            | C-c i / M-g i           |

`epub.el` uses `shr.el` to render html and `epub-mode-map` also inherits keymap from `shr-map` which in turn inherits from `image-map`. This means keybindings defined for image-mode also works under epub-mode.

Perhaps the most common keybindings for images:
| Image                                |           |
|:-------------------------------------|:----------|
| Image increase / decrease size       | i + / i - |
| Image flip horizontally / vertically | i h / i v |
| Image rotate                         | i r       |

### Customization options
- `epub-scroll-pct`
  Percentage of screen height used for scrolling up and down.
- `epub-scroll-beyond`
  Scroll to previous or next page at the beginning or end of the current page.
- `epub-resume-progress`
  Resume reading at the last position when reopening the file.

## Annotations
Features for basic highlighting and commenting are underway.

## Miscellaneous
### xml query
For those interested in learning more about the xml query library in elisp, check out `exml-query.el`. Some basic use cases and tests are provided.

### benchmarks
`<root>/tests/bench-query.el` provides some simple functions for benchmarking and comparing the performances of the two query libraries used by nov.el and epub.el, using the sample epub files under the "<root>/tests/containers/" directory.

