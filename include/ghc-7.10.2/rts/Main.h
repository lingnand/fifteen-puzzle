/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009
 *
 * Entry point for standalone Haskell programs.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTSMAIN_H
#define RTSMAIN_H

/* -----------------------------------------------------------------------------
 * The entry point for Haskell programs that use a Haskell main function
 * -------------------------------------------------------------------------- */

int hs_main (int argc, char *argv[],     // program args
             StgClosure *main_closure,   // closure for Main.main
             RtsConfig rts_config)       // RTS configuration
   GNUC3_ATTRIBUTE(__noreturn__);

#endif /* RTSMAIN_H */