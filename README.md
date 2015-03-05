##esort
esort是一个分布式的erlang 排序服务器，向游戏提供按字段排序的第三方服务

###发起人
* 杨文海 / ywh906@sina.cn ywh906@163.com

##问题背景
假设游戏有一个跨服的活动，按房间分配玩家（每个房间约100~200人），活动开始后玩家进入房间需要显示一个排行榜（比如显示活动积分的前十名），假设DB的结构如下：
CREATE TABLE IF NOT EXISTS `t_worldboat_info`(
        uid                 int(10) unsigned not null comment '玩家的uid',
        serverid            int(10) unsigned not null comment '玩家的服务器id',
        name                varchar(16) not null comment '用户名字',
        level              	int(10) unsigned not null comment '玩家等级',
        score               int(10) unsigned not null comment '玩家积分',
        time                int(10) unsigned not null comment '玩家获得积分的时间(unix时间戳)',
        primary key(uid,serverid)
)engine = InnoDb default charset utf8;

注意，score 只是用来排序，活动已结束就没用了

假设测试数据如下：
+-------+----------+------------+-------+-------+------------+
| uid   | serverid | name       | level | score | time       |
+-------+----------+------------+-------+-------+------------+
| 20106 |        1 | testname-1 |    90 |  1000 | 1423195618 |
| 20107 |        1 | testname-2 |   100 |  1000 | 1423195618 |
| 20108 |        1 | testname-3 |    90 |  2000 | 1423195618 |
| 20109 |        1 | testname-4 |    90 |  3000 | 1423195618 |
| 20110 |        1 | testname-5 |    90 |  3000 | 1423195617 |
+-------+----------+------------+-------+-------+------------+

策划要求排行榜的规则如下：
1、按score 从大到小排序
2、如score相等，则time从小到大排序
3、如果score 和 time 都相同，则按玩家的level从大到小排
4、如果score time level 都相同，则按uid从小到大排序
5、如果score time level uid都相同，则按serverid从小到大排序

实现方案：

方案1、直接用DB的sql实现，sql语句类似如下：
select * from t_worldboat_info order by score desc ,time asc ,level desc,uid asc ,serverid asc ;
+-------+----------+------------+-------+-------+------------+
| uid   | serverid | name       | level | score | time       |
+-------+----------+------------+-------+-------+------------+
| 20110 |        1 | testname-5 |    90 |  3000 | 1423195617 |
| 20109 |        1 | testname-4 |    90 |  3000 | 1423195618 |
| 20108 |        1 | testname-3 |    90 |  2000 | 1423195618 |
| 20107 |        1 | testname-2 |   100 |  1000 | 1423195618 |
| 20106 |        1 | testname-1 |    90 |  1000 | 1423195618 |
+-------+----------+------------+-------+-------+------------+

显然这种方式非常的简单，几条sql就能完成排行榜功能，但是这个方案的问题也比较明显，当玩家人数较多（比如3万），活动开始后成千上万个请求瞬间发到服务器，只是排行榜的请求能把DB压垮，且按照策划要求实现的话是无法使用索引的。而且这个表的规模在较大，排序的sql操作非常耗时。因此在玩家人数较多，瞬间请求较大的情况下这种方法不行。

方案2：分库和分表，注意到这个功能的特殊性，玩家是按房间分配的，我们可以按房间建表，在把这些表分散在多个库里面。每个表的规模就几百，即使不用索引操作也能很快完成，且压力分散到了多个DB，理论上只要DB机器够多，是一定能满足功能需要的。但是这种方案依赖DB机器，分库的策略也不好定，很可能出现某些库负载高某些库负载低，而且应用层的代码编写和线上服务部署也很麻烦。

方案3：使用memcached，将方案2里的每个房间的信息放入memcached，在应用层每个房间会有单独的一个线程来串化处理（使用分布式锁的话性能更低），当应用层要更新一个玩家信息的时候，先从memcached里取出整个房间信息，然后更新，然后转到单独的线程再写入memcached。因为memcached的读写性能非常高，且里面的数据基本有序，应用层使用快速排序后在写回memcached是很快的。这种方案比前面的两种都要好，但是也有一些问题，当高并发大量请求上来后，一个线程可能来不及把信息写入memcached，大量写memcached的操作堆积在线程队列里面，而且应用层从memcached里取到的数据是旧的。但是这种情况比较少，我们通过架构优化能够缓解这种情况发生。
目前我们在线上也是使用的该方案。

方案4：使用第三方服务，该服务能避免上述三种方案的问题，快速的提供排序服务,也就是本文的ersort排序服务。


##特色
    1) 采用erlang编写，erlang天然的进程模型避免了使用锁，无需考虑串化问题
    2) 提供一个proxy客户端，屏保远程服务器，向应用层提供的接口非常简单（使用例子是php写的）
    3) 采用json作为数据交换，只要满足json格式的需求任何语言都可使用
    4) 提供按字段的排序服务，满足灵活的排行榜需求
   

##消息格式

  其他语言和esort通过json通信，消息格式为 4字节的消息头 + json 消息体

##使用的第三方库：
    json解析库：https://github.com/tonyg/erlang-rfc4627
	日志库：https://github.com/qingliangcn/mslog.git
	这两个库都已经集成到了代码里，使用的时候直接make工程即可
	
##架构
esort的架构分为两个部分：
1、esort_server ，核心排序服务器，部署在远程机器上
2、esort_proxy，是应用层和esort_server 沟通的桥梁，esort_proxy直接部署在应用层所在的机器上（比如php-fpm的机器上，php直接跟esort_proxy通信），
   esort_proxy转发应用层的消息到esort_server，server处理完请求后把结果返回给esort_proxy，在由esort_proxy返回给应用层。



##目前提供的接口：
		%%启动并初始化排序服务器
        {init,{"Sortkey","PrimaryKey","SortPriority","TimeOut"}},
        %%将玩家信息更新或者插入到排序列表
        {update,{"Sortkey","Val"}},
        %%获得所有的排序列表
        {getAllSortList,{"Sortkey"}},
        %%获得我的排行
        {getMyRank,{"PrimaryKey","Sortkey"}},
        %%获得排行榜内 从 Start 到 End 的有序列表
        {getTop,{"Sortkey","Start","End"} },
        %%更新或者插入某个玩家的数据，同时返回自己的排行和top列表
        {updateAndGetRank,{"Sortkey","Val","Start","End"}},
        %%获得某个玩家的信息和排名
        {getMyInfo,{"PrimaryKey","Sortkey"}},
        %%获得当前服务器内排序列表的总元素个数
        {getAllCount,{"Sortkey"}},
        %%删除某个玩家的数据
        {delete,{"PrimaryKey","Sortkey"}},
        %%删除所有玩家的数据
        {clearAll,{"Sortkey"}},
        %%停止远程排序服务器
        {destroy ,{"Sortkey"}}
    
##使用方法:

目前该工程只在centos 6.3下测试通过，其他平台暂未测试,且需要php环境（我用的5.3.8）。

1、先安装erlang环境，最简单的办法是去erlang官网下载源码（目前为17.4）：
   http://www.erlang.org/download/otp_src_17.4.tar.gz,解压之后：
   ./config
   sudo make install
   
2、把esort的代码check out 下来，进入代码目录，然后执行 make 即可生成工程

3、修改server.config 和 proxy.config 的端口，目前server用的是7091 proxy用的7090，根据你自己机器的情况修改

4、启动server，在代码目录下执行：
   sh start_server.sh
   
5、启动proxy，在代码目录下执行：
   sh  start_proxy.sh
   
6、使用测试例子，在代码目录下执行：
   php example/Useage.php 

##TODO
压力测试尚未完成
