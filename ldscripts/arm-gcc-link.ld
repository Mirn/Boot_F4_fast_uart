OUTPUT_FORMAT ("elf32-littlearm", "elf32-bigarm", "elf32-littlearm")
/* Internal Memory Map*/
MEMORY
{
	rom (rx)  : ORIGIN = 0x08000000, LENGTH = 0x000F8000
	buf (rwx) : ORIGIN = 0x20000000, LENGTH = 0x0001C000
	ram (rwx) : ORIGIN = 0x2001C000, LENGTH = 0x00004000
	ccm (rw)  : ORIGIN = 0x10008000, LENGTH = 0x00008000
	NOTUSED (rw) : ORIGIN = 0, LENGTH = 0
}

_eram = 0x20000000 + 0x00020000;

__ram_start__  = ORIGIN(ram);
__ram_end__  =  ORIGIN(ram) + LENGTH(ram); 

__stack = ORIGIN(ccm) + LENGTH(ccm);
_sstack = ORIGIN(ccm);
_estack = __stack; 	/* STM specific definition */

__Main_Stack_Size = LENGTH(ccm);
PROVIDE ( _Main_Stack_Size = __Main_Stack_Size ) ;
__Main_Stack_Limit = __stack  - __Main_Stack_Size ;
PROVIDE ( _Main_Stack_Limit = __Main_Stack_Limit ) ;
_Minimum_Stack_Size = 1024;


PROVIDE ( _Heap_Begin = _end_noinit ) ;
PROVIDE ( _Heap_Limit = ORIGIN(ram) + LENGTH(ram)) ;

/* Section Definitions */ 
SECTIONS 
{ 
    .text : 
    { 
	   	. = 0x00000000;
        KEEP(*(.isr_vector .isr_vector.*)) 
        *(.text .text.* .gnu.linkonce.t.*) 	      
        *(.glue_7t) *(.glue_7)		                
        *(.rodata .rodata* .gnu.linkonce.r.*)		    	                  
    } > rom
    
    .ARM.extab : 
    {
        *(.ARM.extab* .gnu.linkonce.armextab.*)
    } > rom
    
    __exidx_start = .;
    .ARM.exidx :
    {
        *(.ARM.exidx* .gnu.linkonce.armexidx.*)
    } > rom
    __exidx_end = .;
    
    .data : AT (_etext) 
    { 
        _sdata = .; 
        *(.data .data.*) 
        . = ALIGN(4); 
        _edata = . ;
    } > ram

    /*. = ALIGN(4);*/
    . = 0x08000000 + 0x2000 - SIZEOF(.data);
    _etext = .;
    _sidata = .;     
    		
    .rxbuf (NOLOAD) : 
    {
    	KEEP(*(.usart_mini_rx_buffer*))
    } > buf
    
    /* .bss section which is used for uninitialized data */ 
    .bss (NOLOAD) : 
    { 
        _sbss = . ; 
        *(.bss .bss.*) 
        *(COMMON) 
        . = ALIGN(4); 
        _ebss = . ; 
    } > ram
    
    .noinit (NOLOAD) :
    {
	    . = ALIGN(4);
        _noinit = .;
        
        *(.noinit .noinit.*) 
        
         . = ALIGN(4) ;
        _end_noinit = .;   
    } > ram    
    
    /* stack section */
    .co_stack (NOLOAD):
    {
        . = ALIGN(8);
        *(.co_stack .co_stack.*)
    } > ccm
    
    . = ALIGN(4); 
    _end = . ; 
} 