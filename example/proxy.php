<?php
define ( 'LOGPATH', dirname ( dirname ( __FILE__ ) ) );
require_once dirname(__file__) ."/Logger.class.php";


class proxy
{
    private $socket;
    private $port;
    private $server;

    function __construct($server,$port)
    {
	$this->server = $server;
	$this->port = $port;
	$this->socket = null;

	//设置日志路径		
	Logger::init (LOGPATH . "/log/php_log/php.log", Logger::L_DEBUG );

    }

    function connect($server, $port)
    {

	    $this->socket = socket_create ( AF_INET, SOCK_STREAM, SOL_TCP );
	    $this->socketError ( 'socket_create', $this->socket );

	    Logger::info ( "connecting to server:%s at port:%d", $server, $port );
	    socket_set_option ( $this->socket, SOL_SOCKET, SO_SNDTIMEO, 
			    array ('sec' => 5, 'usec' => 0 ) );

	    $ret = socket_connect ( $this->socket, $server, $port );
	    $this->socketError ( 'socket_connect', $ret );

	    socket_set_option ( $this->socket, SOL_SOCKET, SO_RCVTIMEO, 
			    array ('sec' => 10, 'usec' => 0 ) );
	    socket_set_option ( $this->socket, SOL_SOCKET, SO_SNDTIMEO, 
			    array ('sec' => 1, 'usec' => 0 ) );
    }

    private function socketError($method, $ret)
    {

	    if ($ret == false)
	    {
		    $errno = socket_last_error ( $this->socket );
		    if ($errno == SOCKET_EINTR)
		    {
			    return;
		    }
		    Logger::fatal ( "%s:%s", $method, socket_strerror ( $errno ) );
		    socket_close ( $this->socket );
		    $this->socket = null;
		    throw new Exception ( 'timeout' );
	    }
    }

    private function writeU32($length)
    {

	    $bytes = '';
	    $bytes .= chr ( ($length & 0xff000000) >> 24 );
	    $bytes .= chr ( ($length & 0x00ff0000) >> 16 );
	    $bytes .= chr ( ($length & 0x0000ff00) >> 8 );
	    $bytes .= chr ( ($length & 0x000000ff) );
	    $this->writeBytes ( $bytes );
    }


    private function writeBytes($bytes)
    {

	    $length = strlen ( $bytes );
	    while ( $length )
	    {
		    $ret = @socket_write ( $this->socket, $bytes );
		    $this->socketError ( 'socket_write', $ret );
		    $bytes = substr ( $bytes, $ret );
		    $length -= $ret;
	    }
    }


    private function readU32()
    {

	    $bytes = $this->readBytes ( 4 );
	    $length = 0;
	    $length += ord ( $bytes [0] ) << 24;
	    $length += ord ( $bytes [1] ) << 16;
	    $length += ord ( $bytes [2] ) << 8;
	    $length += ord ( $bytes [3] );
	    return $length;
    }

    private function readBytes($length)
    {

	    $content = '';
	    while ( $length )
	    {
		    $ret = @socket_read ( $this->socket, $length );
		    $this->socketError ( 'socket_read', $ret );
		    $length -= strlen ( $ret );
		    $content .= $ret;
	    }
	    return $content;
    }

    private function encode($request)
    {
	  return json_encode($request);
    }	
    
    private function decode($request)
    {
          return json_decode($request,true);
    }

    public function getReturnData()
    {

	    while ( true )
	    {
		    $bodyLength = $this->readU32 ();
		     Logger::trace ( "proxy read msg length:%d", $bodyLength);

		    $response = $this->readBytes ( $bodyLength );
		    
		    $json_response=$this->decode($response);

		    Logger::trace ( "proxy read msg body:%s", $json_response);
	
		    if(isset($json_response["Ret"]) && $json_response["Ret"] !="ok")
		    {
			Logger::FATAL("ret err!:%s", $json_response);
			//throw new exception('fake');
		    }

		    return $json_response;
	    }
    }

    function __call($method,$request)
    {
	    if (empty ( $this->socket ))
	    {
		$this->connect ( $this->server, $this->port );
	    }

	    if (empty ( $request )||empty($request[0]))
	    {
		Logger::fatal ( "request empty!method:%s",$method);
		throw new Exception ( 'fatal' );
	    }

	    $request[0]['Method']=$method;
	    Logger::info('input request:%s',$request);

 	    $json_request=$this->encode($request);

	    $this->writeU32 ( strlen ( $json_request) );
	    Logger::trace ( "proxy send msg length:%d", strlen($json_request));

	    $this->writeBytes ( $json_request );
	    Logger::trace ( "proxy send msg body:%s", $json_request);

	
	    $ret=$this->getReturnData ();
	    if(empty($ret))
	    {
		return array();
	    }
	    return $ret[0];
    }

    function __destruct()
    {

		if ($this->socket)
		{
			@socket_close ( $this->socket );
			$this->socket = null;
		}
    }
}
