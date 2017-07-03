# Darkmatter

![Screen Shot](screenshots/screenshot.png)

## Requirement

* <https://github.com/roswell/roswell>
* Your favorite web browser (Google Chrome, Firefox...)
* libev

## Installation

```
# Install requirements

# for macOS
$ brew install libev roswell


# for Arch Linux
$ yaourt install libev roswell
```

```
$ ros install tamamu/darkmatter
```

## Usage

The path starts from current directory.

```
$ cd ~/path/to/root-directory
$ darkm
# Open localhost:8888/browse/file.lisp in your browser!
```

## Symbols

```
*current-directory*
;; The path where the file exists

(enable-infix-syntax)
;; #f(#f(9 + 8 * 2) / 5)

(runtask init-form &body body)
;; Run asynchronous alertable task.
;; The result of body will be alerted finally.

  (checkpoint tmp-form kill-form)
  ;; You can use this form only in runtask.
  ;; If the task should kill, then kill-form will be alerted; otherwise tmp-form will be alerted.
```

## Author

* Eddie (tamamu.1r1s@gmail.com)

## Copyright

Copyright (c) 2017 Eddie (tamamu.1r1s@gmail.com)

## License

Licensed under the MIT License.
