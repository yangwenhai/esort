<?php

$ROOT=dirname(__file__);
require_once "$ROOT/proxy.php";


/*第一步：先初始化排序服务器,调用 init 方法，在调其他方法之前必须先效用init*/
$arr_init = array(
    'Method'=>'init',
    'PrimaryKey'=>array('uid','server_id'),
    'Sortkey'=>'worldboat_room_1',
    'SortPriority'=>array('score'=>'desc','time'=>'asc','level'=>'desc','uid'=>'asc','server_id'=>'asc'),
    'TimeOut'=>3600,//超时时间，这里为1小时
);    

echo "\nmethod:init \n";
$proxy=new proxy('127.0.0.1',7090);
$ret=$proxy->init($arr_init);
if ($ret["Ret"] == "ok") 
{
   $key=$arr_init['Sortkey'];
   echo "init esort server ok!sortkey: $key \n";
}
else
{
   echo "init esort server fail!\n";
   return;
}


/*上面的init初始化好以后就能往远程server里添加或者更新数据了，调用update方法，
  如果server没有主键对应的值，则添加，如果主键对应的值已经存在了则更新,
  上面调用init方法时已经明确指定了 主键为uid 和 server_id,用户可以自定义主键
*/
$arr_update1 = array(
    'Method'=>'update',
    'Sortkey'=>'worldboat_room_1',
    'Val'=>array(
            'uid' => 20106,
            'server_id'=>1,
            'name' => '测试名字1.s1',
            'level'=>90,
            'score'=>1000,
            'time'=>1423195618,),
        );
$arr_update2= array(
    'Method'=>'update',
    'Sortkey'=>'worldboat_room_1',
    'Val'=>array(
            'uid' => 20107,
            'server_id'=>1,
            'name' => 'name2.s1',
            'level'=>100,
            'score'=>1000,
            'time'=>1423195618,),
        );
$arr_update3= array(
    'Method'=>'update',
    'Sortkey'=>'worldboat_room_1',
    'Val'=>array(
            'uid' => 20108,
            'server_id'=>1,
            'name' => 'name3.s1',
            'level'=>90,
            'score'=>2000,
            'time'=>1423195618,),
        );
$arr_update4= array(
    'Method'=>'update',
    'Sortkey'=>'worldboat_room_1',
    'Val'=>array(
            'uid' => 20109,
            'server_id'=>1,
            'name' => '测试名字4.s1',
            'level'=>90,
            'score'=>3000,
            'time'=>1423195618,),
        );
$arr_update5= array(
    'Method'=>'update',
    'Sortkey'=>'worldboat_room_1',
    'Val'=>array(
            'uid' => 20110,
            'server_id'=>1,
            'name' => 'name5.s1',
            'level'=>90,
            'score'=>3000,
            'time'=>1423195618,),
        );
$arr_update6= array(
    'Method'=>'update',
    'Sortkey'=>'worldboat_room_1',
    'Val'=>array(
            'uid' => 20110,
            'server_id'=>1,
            'name' => 'name5.s1',
            'level'=>90,
            'score'=>3001,
            'time'=>1423195619,),
        );

//调用update 6次，server端实际上只有5条数据，第5和6主键一样，第6只是简单更新
echo "\nmethod:update \n";
$proxy=new proxy('127.0.0.1',7090);
$ret1=$proxy->update($arr_update1);
$ret2=$proxy->update($arr_update2);
$ret3=$proxy->update($arr_update3);
$ret4=$proxy->update($arr_update4);
$ret5=$proxy->update($arr_update5);
$ret6=$proxy->update($arr_update6);
if($ret1["Ret"] == "ok" && $ret2["Ret"]="ok" &&
   $ret3["Ret"] == "ok" && $ret4["Ret"]="ok" &&
   $ret5["Ret"] == "ok" && $ret6["Ret"]="ok")
{
   echo "upate ok \n";
}
else
{
   echo "update fail\n";
   return;
}


/*获得所有的排序列表*/
$arr = array(
    'Method'=>'getAllSortList',
    'Sortkey'=>'worldboat_room_1',
);
echo "\nmehotd:getAllSortList ...\n";
$proxy=new proxy('127.0.0.1',7090);
$ret=$proxy->getAllSortList($arr);
var_dump($ret);


/*获得我的排行*/
$arr = array(
    'Method'=>'getMyRank',
    'PrimaryKey'=>array('uid'=>20106,'server_id'=>1),
    'Sortkey'=>'worldboat_room_1',
);
echo "\nmethod:getMyRank \n";
$proxy=new proxy('127.0.0.1',7090);
$ret=$proxy->getMyRank($arr);
var_dump($ret);

/*获得排行榜内 从 Start 到 End 的有序列表*/
$arr = array(
    'Method'=>'getTop',
    'Sortkey'=>'worldboat_room_1',
    'Start'=>1,
    'End'=>3,
);
echo "\nmethod:getTop \n";
$proxy=new proxy('127.0.0.1',7090);
$ret=$proxy->getTop($arr);
var_dump($ret);

/*更新或者插入某个玩家的数据，同时返回自己的排行和top列表*/
$arr = array(
    'Method'=>'updateAndGetRank',
    'Sortkey'=>'worldboat_room_1',
    'Val'=>array(
            'uid' => 20106,
            'server_id'=>1,
            'name' => 'testname1.s1',
            'level'=>90,
            'score'=>1000,
            'time'=>1423195618,),
    'Start'=>1,
    'End'=>3,);
echo "\nmethod:updateAndGetRank \n";
$proxy=new proxy('127.0.0.1',7090);
$ret=$proxy->updateAndGetRank($arr);
var_dump($ret);

/*获得某个玩家的信息和排名*/
$arr = array(
    'Method'=>'getMyInfo',
    'Sortkey'=>'worldboat_room_1',
    'PrimaryKey'=>array('uid'=>20107,'server_id'=>1),);

echo "\nmethod:getMyInfo \n";
$proxy=new proxy('127.0.0.1',7090);
$ret=$proxy->getMyInfo($arr);
var_dump($ret);


/*获得当前服务器内排序列表的总元素个数*/
$arr = array(
    'Method'=>'getAllCount',
    'Sortkey'=>'worldboat_room_1',
);

echo "\nmethod:getAllCount \n";
$proxy=new proxy('127.0.0.1',7090);
$ret=$proxy->getAllCount($arr);
var_dump($ret);

/*删除某个玩家的数据*/
$arr = array(
    'Method'=>'delete',
    'Sortkey'=>'worldboat_room_1',
    'PrimaryKey'=>array('uid'=>20107,'server_id'=>1),);

echo "\nmethod:delete \n";
$proxy=new proxy('127.0.0.1',7090);
$ret=$proxy->delete($arr);
var_dump($ret);

/*删除所有玩家的数据*/
$arr = array(
    'Method'=>'clearAll',
    'Sortkey'=>'worldboat_room_1',
);

echo "\nmethod:clearAll \n";
$proxy=new proxy('127.0.0.1',7090);
$ret=$proxy->clearAll($arr);
var_dump($ret);

/*停止远程排序服务器并删除所有数据*/
$arr = array(
    'Method'=>'destroy',
    'Sortkey'=>'worldboat_room_1',
);

echo "\nmethod:destroy \n";
$proxy=new proxy('127.0.0.1',7090);
$ret=$proxy->destroy($arr);
var_dump($ret);


