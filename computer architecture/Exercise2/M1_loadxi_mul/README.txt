Quick start:  The Sigma16 M1 CPU circuit
See user guide in docs directory

------------------------------------------------------------------------
*** Running the ALU simulator on its own

  $ cd M1
  $ ghc -e main Circuit/ALUrun

------------------------------------------------------------------------
*** Running a machine language program on the M1 CPU circuit

The object code must be in Sigma16 object format, which is produced
by the Sigma16 assembler.  Assembling source program Add.asm.txt
will produce the machine language code in Add.obj.txt.
  
  $ ghci
  ghci> :load Run
  ghci> :main programs/Add
  M1> run
  M1> quit
  ghci> :quit

Using breakpoints and register/memory display

  $ ghci
  ghci> :load Run
  ghci> main programs/ArrayMax
  M1> break reset    set breakpoint
  M1> run            run until reset=1
  M1> (enter)        run just one  clock cycle
  M1 run
  M1> regs           print the contents of the register file
  M1> mem 0 30       print memory from address 0 to 30
