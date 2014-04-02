Linktester
=======

Tool written in haskell, mainly as a learning effort. It spiders a website to find broken links (and off-site links to specific domains)

```
Usage: linktester link [OPTION...]
example: linktester http://www.google.com/ -f http://youtube.com/ -w http://google.com/jobs
  -w url  --filter=url  don't go into urls starting with this
  -o url  --only=url    do go into urls starting with this
  -f url  --follow=url  check urls starting with this, but don't also check links in them
```

Installation
=====

```sh
git clone https://github.com/yorickvP/linktester
cd linktester
cabal install
```
