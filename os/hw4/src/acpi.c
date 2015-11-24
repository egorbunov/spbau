#include "acpi.h"

#include <stddef.h>
#include "string.h"
#include "vga.h"



bool acpi_do_checksum(acpi_sdt_header_t *tableHeader)
{
    unsigned char sum = 0;
    for (int i = 0; i < tableHeader->length; i++) {
        sum += ((char *) tableHeader)[i];
    }
    return sum == 0;
}

/* 
* get root system descriptor
*/
bool acpi_get_rsd(rsdp_descriptor_t* prsdDescriptor) {
    const char* RSD_PTR_STR = "RSD PTR ";

    char* memptr = 0;
    int rsd_start;

    // trying to find in EBDA
    const int EBDA_PTR_LOCATION = 0x40e << 4; // mult with 16...
    int EBDA_ADDR = (* (int*)(EBDA_PTR_LOCATION));
    memptr = (char*)(EBDA_ADDR);

    rsd_start = str_find(memptr, RSD_PTR_STR, 0, 1024);

    if (rsd_start >= 0) {
        *prsdDescriptor = *((rsdp_descriptor_t*) (memptr + rsd_start));
        return true;
    }

    // trying to find in memory region...
    const int START_ADDR = 0x000E0000;
    const int END_ADDR = 0x000FFFFF;
    memptr = (char*) START_ADDR;
    rsd_start = str_find(memptr, RSD_PTR_STR, 0, END_ADDR - START_ADDR);

    if (rsd_start < 0) {
        return false;
    }

    *prsdDescriptor = *((rsdp_descriptor_t*) (memptr + rsd_start));
    return true;
}

acpi_sdt_header_t* acpi_get_sdt(rsdt_t* p_rsdt, const char* signature) {
    int sdt_num = (p_rsdt->h.length - sizeof(acpi_sdt_header_t)) / 4;
    // printf("SDT NUM = %d\n", sdt_num);

    acpi_sdt_header_t* p_cur_sdt_h = (acpi_sdt_header_t*) p_rsdt->sdts;
    for (int i = 0; i < sdt_num; ++i) {

        // printf("ADDRESS = %d (%x) \n", p_cur_sdt_h, p_cur_sdt_h);

        if (!acpi_do_checksum(p_cur_sdt_h)) {
            printf("Wrong checksum for sdt at address = %x", p_cur_sdt_h);
        }

        if (strcmp(p_cur_sdt_h->signature, signature, 0, 4)) {
            return p_cur_sdt_h;
        }

        // printf("SIGNATURE #%d = \"", i);
        // prints((((acpi_sdt_header_t*) p_cur_sdt_h)->signature), 0, 4);
        // printf("\"; length = %d\n", ((acpi_sdt_header_t*) p_cur_sdt_h)->length);

        p_cur_sdt_h = (acpi_sdt_header_t*) (((uint8_t*) p_cur_sdt_h) + ((acpi_sdt_header_t*) p_cur_sdt_h)->length);
    }

    return NULL;
}
