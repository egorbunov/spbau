#ifndef ACPI_H_INCLUDED__
#define ACPI_H_INCLUDED__

#include <stdbool.h>
#include <stdint.h>

//ROOT SYSTEM DESCRIPTOR POINTER
typedef struct __attribute__ ((__packed__)) {
	char signature[8];
	uint8_t checksum;
	char oem_id[6];
	uint8_t revision;
	uint32_t rsdt_address;
} rsdp_descriptor_t;

// *******************  SDT's  *******************

typedef struct __attribute__ ((__packed__)) {
	char signature[4];
	uint32_t length;
	uint8_t revision;
	uint8_t checksum;
	char oem_id[6];
	char oem_table_id[8];
	uint32_t oem_revision;
	uint32_t creator_id;
	uint32_t creator_revision;
} acpi_sdt_header_t;

// ROOT SYSTEM DESCRIPTOR TABLE
typedef struct __attribute__ ((__packed__)) {
	acpi_sdt_header_t h;
	uint32_t* sdts;
} rsdt_t;

// MULTIPLE APIC DESCRIPTOR TABLE and stuff...
typedef struct __attribute__ ((__packed__)) {
	uint8_t type;
	uint8_t len;
} apic_struct_header_t;

typedef struct __attribute__ ((__packed__)) {
	acpi_sdt_header_t h;
	uint32_t local_apics_addr;
	uint32_t flags;
	apic_struct_header_t first_int_controller; // head for interrupt controller list
} madt_t;


// ***********************************************

// ************** Interrupt controller's structs ***************

enum APIC_STRUCTURE_TYPE {
	PROC_LOCAL_APIC, // 0 
	IO_APIC, // 1
	INT_SOURCE_OVERRIDE,
	NMI, // Non-maskable Interrupt Source (NMI)
	LOCAL_APIC_NMI,
	LOCAL_APIC_ADDR_OVERRIDE, // 5
	IO_SAPIC,
	LOCAL_SAPIC,
	PLATF_INT_SOURCES,
	PROC_LOCAL_x2APIC, // 9
	LOCAL_x2APIC_NMI,
	GIC,
	GICD
};

typedef struct __attribute__ ((__packed__)) {
	apic_struct_header_t h;
	uint8_t acpi_proc_id;
	uint8_t id;
	uint32_t flags;
} proc_local_apic_t;

typedef struct __attribute__ ((__packed__)) {
	apic_struct_header_t h;
	uint8_t id;
	uint8_t reserved;
	uint32_t addr;
	uint32_t glob_sys_int_base;
} io_apic_t;

// local apic address overide
typedef struct __attribute__ ((__packed__)) {
	apic_struct_header_t h;
	uint16_t reserved;
	uint64_t addr;
} loc_apic_addr_ov_t;

typedef struct __attribute__ ((__packed__)) {
	apic_struct_header_t h;
	uint16_t reserved;
	uint32_t id;
	uint32_t flags;
	uint32_t proc_uid;
} proc_local_x2apic_t;

// *************************************************************

bool acpi_do_checksum(acpi_sdt_header_t *table_header);
bool acpi_get_rsd(rsdp_descriptor_t* prsd_descriptor);
acpi_sdt_header_t* acpi_get_sdt(rsdt_t* p_rsdt, const char* signature);

#endif