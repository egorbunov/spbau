#ifndef MULTIBOOT_H_INCLUDED__
#define MULTIBOOT_H_INCLUDED__

/* The Multiboot header. */
typedef struct multiboot_header
{
	uint32_t magic;
	uint32_t flags;
	uint32_t checksum;
	uint32_t header_addr;
	uint32_t load_addr;
	uint32_t load_end_addr;
	uint32_t bss_end_addr;
	uint32_t entry_addr;
} multiboot_header_t;

/* The symbol table for a.out. */
typedef struct multiboot_aout_symbol_table
{
	uint32_t tabsize;
	uint32_t strsize;
	uint32_t addr;
	uint32_t reserved;
} multiboot_aout_symbol_table_t;

/* The section header table for ELF. */
typedef struct multiboot_elf_section_header_table
{
	uint32_t num;
	uint32_t size;
	uint32_t addr;
	uint32_t shndx;
} multiboot_elf_section_header_table_t;

 /* The Multiboot information. */
typedef struct multiboot_info
{
	uint32_t flags;
	uint32_t mem_lower;
	uint32_t mem_upper;
	uint32_t boot_device;
	uint32_t cmdline;

	uint32_t mods_count;
	uint32_t mods_addr;
	union
	{
		multiboot_aout_symbol_table_t aout_sym;
		multiboot_elf_section_header_table_t elf_sec;
	} u;

	uint32_t mmap_length;
	uint32_t mmap_addr;
} multiboot_info_t;
 
/* The memory map. Be careful that the offset 0 is base_addr_low but no size. */
typedef struct memory_map
{
	unsigned long size;
	unsigned long base_addr_low;
	unsigned long base_addr_high;
	unsigned long length_low;
	unsigned long length_high;
	unsigned long type;
} memory_map_t;

#endif