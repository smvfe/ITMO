#!/bin/bash
javac --class-path .. Server.java Client.java BankWebServer.java
#rmic -d $CLASSPATH examples.rmi.RemoteAccount examples.rmi.RemoteBank
