@echo off
call javac --class-path .. Server.java Client.java BankWebServer.java
rem call rmic -d .. examples.rmi.RemoteAccount examples.rmi.RemoteBank
