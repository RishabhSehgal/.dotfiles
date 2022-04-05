# DOTFILES Repository personal Macbook


## SUBTREE .emacs.d
Used the instructions in the following webpages to use subtree merge strategy for .emacs.d
This repo is coming from https://github.com/hlissner/doom-emacs.git

* https://mirrors.edge.kernel.org/pub/software/scm/git/docs/howto/using-merge-subtree.html
* https://www.atlassian.com/git/tutorials/git-subtree

### Used following commands
1. git remote add -f emacs.d https://github.com/hlissner/doom-emacs
2. git merge -s ours --no-commit --allow-unrelated-histories emacs.d/master
3. git read-tree --prefix=.emacs.d/ -u emacs.d/master
4. git commit -m "Merge emacs.d project as our subdirectory"

5. git pull -s subtree emacs.d master
