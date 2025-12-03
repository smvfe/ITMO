@echo off
set classpath=..
start rmiregistry
start java rmi.Server
