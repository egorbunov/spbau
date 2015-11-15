#include <stdint.h> 
#include "util.h"
#include "vga.h"
#include "serial.h"
#include "string.h"
#include "multiboot.h"
#include "acpi.h"

void print_mboot_info(multiboot_info_t* pmbinfo) {
    if (CHECK_FLAG (pmbinfo->flags, 2)) {
        printf("Command line: %s\n", pmbinfo->cmdline);
    } else {
        printf("Command line not passed\n");
    }
    printf("============================================\n");
    /* Are mmap_* valid? */
    if (CHECK_FLAG (pmbinfo->flags, 6)) {
        multiboot_memory_map_t *mmap;
        for (mmap = (multiboot_memory_map_t *) pmbinfo->mmap_addr; 
            (unsigned long) mmap < pmbinfo->mmap_addr + pmbinfo->mmap_length;
            mmap = (multiboot_memory_map_t *) ((uint32_t) mmap + mmap->size + sizeof(mmap->size)))
            printf ("memory range = 0x%x%x - 0x%x%x, type = %d\n",
                    (uint32_t) (mmap->base >> 32),
                    (uint32_t) (mmap->base & 0xffffffff),
                    (uint32_t) ((mmap->base + mmap->len - 1) >> 32),
                    (uint32_t) ((mmap->base + mmap->len - 1) & 0xffffffff),
                    mmap->type);
    } else {
        printf("Memory info is not available\n");
    }
}

void cmain(unsigned long magic, multiboot_info_t* pmbinfo) {
    init_vga();

    printf("============================================\n");
    print_mboot_info(pmbinfo);
    printf("============================================\n");

    rsdp_descriptor_t rsd_descriptor; 
    if (!acpi_get_rsd(&rsd_descriptor)) {
        printf("Cannot get RSD structure!\n");
    }

    // getting rsdt
    rsdt_t* p_rsdt = (rsdt_t*) rsd_descriptor.rsdt_address;

    // getting madt
    madt_t* p_madt = (madt_t*) acpi_get_sdt(p_rsdt, "APIC"); 

    if (p_madt == NULL) {
        printf("ERROR: madt not found!\n");
    } else {
        
        int64_t local_apics_addr = p_madt->local_apics_addr;

        apic_struct_header_t* p_cur_apic_struct_head = &(p_madt->first_int_controller);
        int apic_structs_len = (p_madt->h.length - sizeof(*p_madt) + sizeof(p_madt->first_int_controller));
        // iterating through int controller structures list
        while (apic_structs_len > 0) {
            apic_structs_len -= p_cur_apic_struct_head->len;
            switch (p_cur_apic_struct_head->type) {
                case PROC_LOCAL_APIC: {
                    proc_local_apic_t* p_apic = (proc_local_apic_t*) p_cur_apic_struct_head; 
                    printf("Processor Local APIC [id = %d]\n", p_apic->id);
                    break;
                }
                case IO_APIC: {
                    io_apic_t* p_apic = (io_apic_t*) p_cur_apic_struct_head; 
                    printf("IOAPIC [%d] at [0x%x] IRQs from [%d]\n", p_apic->id, p_apic->addr, p_apic->glob_sys_int_base);
                    break;
                }
                case LOCAL_APIC_ADDR_OVERRIDE: {
                    loc_apic_addr_ov_t* p_apic = (loc_apic_addr_ov_t*) p_cur_apic_struct_head; 
                    local_apics_addr = p_apic->addr;
                    break;
                }
                case PROC_LOCAL_x2APIC: {
                    proc_local_x2apic_t* p_apic = (proc_local_x2apic_t*) p_cur_apic_struct_head; 
                    printf("Processor Local x2APIC [id = %d]\n", p_apic->id);
                    break;
                }
                default:
                    break;
            }
            // next apic
            p_cur_apic_struct_head =
                 (apic_struct_header_t*) (((uint8_t*) p_cur_apic_struct_head) + p_cur_apic_struct_head->len);
        }

        printf("\n");
        if (local_apics_addr > 0) {
            printf("Local APICs accessible at [0x%x%x]\n", 
                (uint32_t) (local_apics_addr >> 32), 
                (uint32_t) (local_apics_addr & 0xffffffff));
        } else {
            printf("ERROR: Cannot get local apics address\n");
        }

        // very first bit is PCAT_COMPAT flag
        if (CHECK_FLAG(p_madt->flags, 0)) {
            printf("PC/AT dual PIC supported\n");
        } else {
            printf("PC/AT dual PIC not supported\n");
        }
    }

    printf("============================================\n");
    while(1) { };
}