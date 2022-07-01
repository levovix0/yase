
<img alt="icon" align="left" width="70" src="http://levovix.ru/resources/yase/icon.svg">
<p>
  <h3>Yase: yet another self-editor</h3>
  Self-editor, inspired by  <a href="https://github.com/mastertimer/mutator">mutator</a>
</p>

![screenshot](http://levovix.ru/resources/yase/screenshots/0.1.png)

# Installation
Install [Nim](https://nim-lang.org/install.html) compiler first.
```sh
git clone https://github.com/levovix0/yase
cd yase
nimble -d:release build
```
### Usage
```sh
./yase
```
type `h` for help

# Differences of yase and mutator
* nodes (tetrons) in yase declared as
  ```nim
  type Node = ref object
    kind: Node
    childs: seq[Node]
    data: seq[byte]
  ```
  when in mutator tetrons declared as hierarchy of types, based on (roughly)
  ```nim
  type
    Flag = uint64
    Tetron = ref object of RootObj
      links: Table[Tetron, Flag]
  ```
  (types based on Tetron may have additional data fields)

* for now, yase is console program

* basic yase interpretator writed in Nim, instead of C++
