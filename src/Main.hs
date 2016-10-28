-- Skeleton taken from https://wiki.haskell.org/Implement_a_chat_server

module Main where

import Network.Socket
import System.IO 
import Control.Concurrent
import Control.Concurrent.Chan

import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)

import Control.Exception

type Message = (Int, String)

main :: IO ()
main = do 
	
	sock <- socket AF_INET Stream 0           -- Create a socket
	setSocketOption sock ReuseAddr 1          -- Make socket reusable
	bind sock (SockAddrInet 4242 iNADDR_ANY)  -- Bind sock to tcp 4242
	listen sock 5                             -- Maximum of two connections

	channel <- newChan

	forkIO $ fix $ \loop -> do 
		(_, msg) <- readChan channel
		loop

	mainLoop sock channel 0  

mainLoop :: Socket -> Chan Message -> Int -> IO () 
mainLoop sock channel num = do 
	conn <- accept sock 					  -- Accept the incoming connection
	forkIO (handleClient conn channel num)    -- Handle the connection, fork
	mainLoop sock channel $! num + 1

handleClient :: (Socket, SockAddr) -> Chan Message -> Int -> IO () 
handleClient (sock, _) channel num = do 

	let broadcast msg = writeChan channel (num, msg)
	hdl <- socketToHandle sock ReadWriteMode 
	hSetBuffering hdl NoBuffering

	hPutStrLn hdl "Hello friend. Your name: "
	name <- liftM init (hGetLine hdl)
	broadcast ("--> " ++ name ++ " online.")
	hPutStrLn hdl ("Welcome, " ++ name)

	readChannel <- dupChan channel

	-- Get a thread to read from the channel
	reader <- forkIO $ fix $ \loop -> do 
		(nextNum, line) <- readChan readChannel
		when (num /= nextNum) $ hPutStrLn hdl line
		loop

	handle (\(SomeException _) -> return ()) $ fix $ \loop -> do 
		line <- liftM init (hGetLine hdl) 
		case line of
			"quit" -> hPutStrLn hdl "Bye!"
			_      -> broadcast (name ++ ": " ++ line) >> loop

   	killThread reader
   	broadcast (name ++ "left.")
	close sock

