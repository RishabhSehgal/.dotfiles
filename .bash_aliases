## rishabhs added 08/24/2019 ########

#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'
alias sa='source ~/.bash_aliases && echo "sourced rishabhs bash aliases"'
alias ll='ls -ltrh --color=auto'
alias cdw='cd $WARD'
alias vr='gvim -R $1'
alias tdir='dt=$(date "+%m_%d_%y_%H%M") && mkdir $1_$dt'
alias rmr='rm -r $1 && echo "removed directory $1"'
alias rp='realpath $1'
alias kgnome='killall -HUP -u $USER gnome-shell'
alias pomo='get-pomodori -b 7 -p 40 -l 15 -g 3 -x $1'
alias pomoss='get-pomodori -b 7 -p 40 -l 15 -g 3 -x -f $1 $2'
alias jn='jupyter notebook'
alias nv='nvidia-smi'

alias sb='subl $1'
alias sbr='subl -R $1'
alias maxlx='module avail xilinx'
alias mld='module load $1'
alias muld='module unload $1'
alias vvdo='vivado &'
alias tdir='dt=$(date "+%m_%d_%y_%H%M") && mkdir $1_$dt'
alias rmr='rm -r $1 && echo "removed directory $1"'
alias rp='realpath $1'
alias lpr='lpr -P copier -o KmManagement=8312 -o sides=two-sided-long-edge -o page-ranges=1-20 $1'
alias lpr4pg='lpr -P copier -o KmManagement=8312 -o orientation-requested=4 -o page-ranges=12-30 -o sides=two-sided-long-edge -o number-up=2 $1'
alias lpr2pg='lpr -P copier -o KmManagement=8312 -o orientation-requested=4 -o page-ranges=1-20 -o sides=two-sided-long-edge -o number-up=2 $1'
alias viggy='cd /home/local/eta/vigneshrk'
#setxkbmap -rules xorg -model pc105 -layout eu -option ""
alias df='df -h ~'
alias lambda='cd //home/usr1/rishabhs'
alias localbig='cd //home/local/big/rishabhs'
alias trn='cd ~rishabhs/Documents/UTA/Research/Incremental\ Learning\ papers/Training'
alias localhome='cd /home/local/lambda/rishabhs'
alias pycharm='/home/local/lambda/rishabhs/binaries/Binaries/pycharm-community-2019.3.1/pycharm-community-2019.3.1/bin/pycharm.sh &'
alias ml='cd /home/local/lambda/rishabhs/ML'
alias mt='matlab -softwareopengl &'
alias shirinb='cd /home/local/big/asayal/shirin_binary'
alias cim='cd ~rishabhs/Documents/UTA/Research/CiM'
alias theta='ssh -Y -p 222 rishabhs@theta.cerc.utexas.edu'

###### DOOM aliases ######
alias dd='doom doctor'
alias ds='doom sync'
alias dds='doom sync; doom doctor'


###### Git aliases ######
alias gits='git status'
alias gitl='git log'
alias gita='git add $1'
alias gitaa='git add .'
alias gitc='git commit'
alias graph='git log --all --decorate --oneline --graph'
alias gitb='git branch $1'

###### Conda env and cuda related aliases ######
alias cuda='source ~/scripts/switch-cuda.sh'
alias condaenv='source ~/scripts/shell/create-conda-env.sh'
alias condaexp='conda env export | grep -v "^prefix: " > environment.yml'



function myinfo () {
  printf "CPU: "
  cat /proc/cpuinfo | grep "model name" | head -1 | awk '{ for (i = 4; i <= NF; i++) printf "%s ", $i }'
  printf "\n"

  cat /etc/issue | awk '{ printf "OS: %s %s %s %s | " , $1 , $2 , $3 , $4 }'
  uname -a | awk '{ printf "Kernel: %s " , $3 }'
  uname -m | awk '{ printf "%s | " , $1 }'
  kded4 --version | grep "KDE Development Platform" | awk '{ printf "KDE: %s", $4 }'
  printf "\n"
  uptime | awk '{ printf "Uptime: %s %s %s", $3, $4, $5 }' | sed 's/,//g'
  printf "\n"
  cputemp | head -1 | awk '{ printf "%s %s %s\n", $1, $2, $3 }'
  cputemp | tail -1 | awk '{ printf "%s %s %s\n", $1, $2, $3 }'
  nvidia-smi
  #cputemp | awk '{ printf "%s %s", $1 $2 }'
}

