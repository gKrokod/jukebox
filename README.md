TODO
1. [done] Exception-safe ffplay lifecycle (Ctrl+C and console close button).
   ffplay is now spawned/reaped with bracket in Engine.playTrackSTM, and on
   Windows a native console-control handler (Player.Shutdown.withShutdownHandler)
   kills ffplay on CTRL_CLOSE_EVENT before the OS terminates the process.
4. Random for eqally track in playlist.


