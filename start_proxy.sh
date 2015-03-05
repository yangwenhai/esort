#! /bin/bash


#kill掉已经存在的进程
function kill_esort_proxy()
{
   processpid=`ps -ef |grep esort_proxy |grep erlang|grep -v grep |awk '{print $2}'|tail -1`
   if [[ ! -z "$processpid" ]] ; then
        kill -9 $processpid
   fi
}


#进入工程目录
SELF=`readlink -f "$0"`
WORKDIR=`dirname "$SELF"`
cd "$WORKDIR"


#检查是否已经启动了
running=`ps -ef | grep esort_proxy |grep erlang|grep -v grep| wc -l`
if [[ $running -ge 1 ]]; then
    kill_esort_proxy
fi

#看看端口是不是已经被别的进程占用了
PORT=`cat proxy.config |grep local_port|grep -Eo "[0-9]{1,}"`
checkflag=`netstat -nltp 2>&1|grep $PORT|grep -v beam|grep LISTEN`
if [[ ! -z "$checkflag" ]] ;then 
   echo "fail!port:$PORT alreay be used!"
   exit
fi

#排序服务器是否已经启动
checkserver=`ps -ef | grep esort_server |grep erlang|grep -v grep| wc -l`
if [[ $checkserver -lt 1 ]]; then
    echo "fail!need to start esort_server !"
    exit
fi

#启动服务
PZ='-pz common/ebin/json/ -pz common/ebin/mslog/ -pz common/ebin/'
erl -boot start_sasl  -pa esort_proxy/ebin $PZ -config proxy +K true -s esort_proxy start -detached

#sleep一下，服务可能还没启动完成
sleep 5

#检查是否启动成功(检查服务端口，看是否能建立连接)
nc -v -w 3 -z  127.0.0.1  $PORT > /dev/null 2>&1
ret=$?
if [[ $ret -ne 0 ]]; then

   kill_esort_proxy
   echo "fail! can not start esort_proxy! please check port:$PORT!"

else
   echo "start esort_proxy ok!"
fi
